# shellcheck disable=SC2154

set -eo pipefail

candidates=("$1-unwrapped" "$1")

for installable in "${candidates[@]}"; do
  while read -r line; do
    # shellcheck disable=SC2163
    export "$line"
  done < <(nix eval --json --read-only "$installable" --apply "c:
  let
    NID_INSTALLABLE = if c ? unwrapped then \"$installable.unwrapped\" else \"$installable\";
    p = if c ? unwrapped then c.unwrapped else c;
  in {
    inherit NID_INSTALLABLE;
    NID_OUT = p.pname or p.name;
    unpackPhase = p.unpackPhase or \"\";
    src_name = p.src.name or \"\";
    src_git_url = p.src.gitRepoUrl or \"\";
    src_rev = p.src.rev or \"\";
    src_fetch_submodules = p.src.fetchSubmodules or false;
  }" 2>/dev/null | jq -r "to_entries|map(\"\(.key)=\(.value|tostring)\")|.[]")

  if [ -n "$NID_INSTALLABLE" ]; then
    break
  fi
done

if [ -z "$NID_INSTALLABLE" ]; then
  echo "Invalid installable: $1"
  exit 1
fi

if [ "$NID_INSTALLABLE" != "$1" ]; then
  echo "Unwrapping $1 → $NID_INSTALLABLE"
fi

NID_OUT="$PWD/${NID_OUT%-unwrapped}"
echo "Unpacking $NID_INSTALLABLE → $NID_OUT"
mkdir "$NID_OUT"

if [ -z "$unpackPhase" ] && [ -n "$src_git_url" ]; then
  # Shallow clone to rev
  cd "$NID_OUT"
  git init
  git remote add origin "$src_git_url"
  git fetch --depth 1 origin "$src_rev"
  git reset --hard FETCH_HEAD

  # Fetch submodules if necessary
  if [ "$src_fetch_submodules" = "true" ]; then
    git submodule update --init --recursive -j "$(nproc)" --progress --depth 1
  fi

  # Fetch all other commits in the background
  git fetch --all --unshallow --quiet &

  # Override unpackPhase
  unpackPhase="
runHook preUnpack
ln -s $NID_OUT $src_name

if [ -n \"${setSourceRoot:-}\" ]; then
  runOneHook setSourceRoot
fi

sourceRoot=${sourceRoot:-$src_name}
runHook postUnpack"
else
  unset unpackPhase
fi

unset src_name src_git_url src_rev src_fetch_submodules

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
