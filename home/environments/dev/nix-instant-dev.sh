# shellcheck disable=SC2154

set -eo pipefail

for NID_INSTALLABLE in "$1-unwrapped" "$1"; do
  while read -r line; do
    # shellcheck disable=SC2163
    export "$line"
  done < <(nix eval --json --read-only "$NID_INSTALLABLE" --apply 'c:
  let
    unwrapped = c ? unwrapped;
    p = if unwrapped then c.unwrapped else c;
  in {
    inherit unwrapped;
    NID_OUT = p.pname or p.name;
    unpackPhase = p.unpackPhase or "";
    srcName = p.src.name or "";
    srcGitRepoUrl = p.src.gitRepoUrl or "";
    srcRev = p.src.rev or "";
    srcFetchSubmodules = p.src.fetchSubmodules or false;
  }' 2>/dev/null | jq -r "to_entries|map(\"\(.key)=\(.value|tostring)\")|.[]")

  if [ -n "$NID_OUT" ]; then
    break
  fi
done

if [ -z "$NID_OUT" ]; then
  echo "Invalid installable: $1"
  exit 1
fi

if [ "$unwrapped" = "true" ]; then
  NID_INSTALLABLE="$NID_INSTALLABLE.unwrapped"
fi

if [ "$NID_INSTALLABLE" != "$1" ]; then
  echo "Unwrapping $1 → $NID_INSTALLABLE"
fi

NID_OUT="$PWD/${NID_OUT%-unwrapped}"
echo "Unpacking $NID_INSTALLABLE → $NID_OUT"
mkdir "$NID_OUT"

if [ -z "$unpackPhase" ] && [ -n "$srcGitRepoUrl" ]; then
  # Shallow clone to rev
  cd "$NID_OUT"
  git init
  git remote add origin "$srcGitRepoUrl"
  git fetch --depth 1 origin "$srcRev"
  git reset --hard FETCH_HEAD

  # Fetch submodules if necessary
  if [ "$srcFetchSubmodules" = "true" ]; then
    git submodule update --init --recursive -j "$(nproc)" --progress --depth 1
  fi

  # Fetch all other commits in the background
  git fetch --all --unshallow --quiet &

  # Override default unpackPhase
  unpackPhase="
runHook preUnpack
ln -s \"\$NID_OUT\" $srcName

if [ -n \"\${setSourceRoot:-}\" ]; then
  runOneHook setSourceRoot
fi

sourceRoot=\${sourceRoot:-$srcName}
runHook postUnpack"
else
  unset unpackPhase
fi

unset unwrapped srcName srcGitRepoUrl srcRev srcFetchSubmodules

# shellcheck source=/dev/null
. <(nix print-dev-env "$NID_INSTALLABLE")
export NIX_LOG_FD=/dev/null
cd "$NIX_BUILD_TOP"
phases="${prePhases[*]:-} unpackPhase" genericBuild
if ! [ -L "$NIX_BUILD_TOP/${sourceRoot:-.}" ]; then
  cd "$(dirname "$NID_OUT")"
  mv -T "$NIX_BUILD_TOP/${sourceRoot:-.}" "$NID_OUT"
  ln -s "$NID_OUT" "$NIX_BUILD_TOP/${sourceRoot:-.}"
fi

cd "$NID_OUT"
cmakeFlags="-Cnix-instant-dev.cmake $cmakeFlags"
phases="patchPhase ${preConfigurePhases[*]:-} configurePhase" genericBuild

if [ -e compile_commands.json ]; then
  ln -rs compile_commands.json "$NIX_BUILD_TOP/$sourceRoot"
fi

rm -rf "$NIX_BUILD_TOP"
wait
