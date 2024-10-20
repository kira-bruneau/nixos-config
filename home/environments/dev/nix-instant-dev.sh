set -eo pipefail

unwrapped=
name=
src=
srcName=
srcGitRepoUrl=
srcRev=
srcFetchSubmodules=
unpackPhase=

# 1. Try to extract source metadata from Nix expression
for NID_INSTALLABLE in "$1-unwrapped" "$1"; do
  eval "$(nix eval --json --read-only "$NID_INSTALLABLE" --apply 'c:
    let
      unwrapped = c ? unwrapped;
      p = if unwrapped then c.unwrapped else c;
    in {
      inherit unwrapped;
      name = p.pname or (builtins.parseDrvName p.name).name;
      srcName = p.src.name or "";
      srcGitRepoUrl = p.src.gitRepoUrl or "";
      srcRev = p.src.rev or "";
      srcFetchSubmodules = p.src.fetchSubmodules or false;
      unpackPhase = p.unpackPhase or "";
    }' 2>/dev/null | jq -r 'to_entries[] | "export " + @sh "\(.key)=\(.value)"')"

  if [ -n "$name" ]; then
    if [ "$unwrapped" = "true" ]; then
      NID_INSTALLABLE="$NID_INSTALLABLE.unwrapped"
    fi

    break
  fi
done

# 2. Try to extract source metadata from Nix derivation
if [ -z "$name" ]; then
  eval "$(nix derivation show "$NID_INSTALLABLE" 2>/dev/null | jq -r 'to_entries[] | {
    NID_INSTALLABLE: (.key),
    name: (.value.env.pname // .value.name),
    src: (.value.env.src // ""),
    unpackPhase: (.value.env.unpackPhase // ""),
  } | to_entries[] | "export " + @sh "\(.key)=\(.value)"')"

  if [ -n "$src" ]; then
    eval "$(nix derivation show "$src" 2>/dev/null | jq -r 'to_entries[] | {
      srcName: .value.name,
      srcGitRepoUrl: (if .value.env.fetcher // "" | endswith("nix-prefetch-git") then .value.env.url else "" end),
      srcRev: (.value.env.rev // ""),
      srcFetchSubmodules: (.value.env.fetchSubmodules // false),
    } | to_entries[] | "export " + @sh "\(.key)=\(.value)"')"
  fi

  if [ -z "$name" ]; then
    echo "Invalid installable: $1"
    exit 1
  fi
fi

if [ "$NID_INSTALLABLE" != "$1" ]; then
  echo "Resolving $1 → $NID_INSTALLABLE"
fi

NID_OUT="$PWD/${name%-unwrapped}"
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

unset unwrapped
unset name
unset src
unset srcName
unset srcGitRepoUrl
unset srcRev
unset srcFetchSubmodules

# shellcheck source=/dev/null
. <(nix print-dev-env "$NID_INSTALLABLE")
export NIX_LOG_FD=/dev/null
cd "$NIX_BUILD_TOP"
cmakeFlags="-Cnix-instant-dev.cmake $cmakeFlags"
phases="${prePhases[*]:-} unpackPhase patchPhase ${preConfigurePhases[*]:-} configurePhase" genericBuild

if [ -e compile_commands.json ]; then
  ln -rs compile_commands.json "$NID_OUT"
fi

rm -rf "$NIX_BUILD_TOP"
wait
