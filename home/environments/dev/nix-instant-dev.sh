set -eo pipefail

installable=$1

NID_OUT=$(readlink -f "$(nix eval --raw "$installable".pname 2>/dev/null || nix eval --raw "$installable".name)")
mkdir "$NID_OUT"

if ! nix eval --raw "$installable".unpackPhase 2>/dev/null; then
  if src_git_url=$(nix eval --raw "$installable".src.gitRepoUrl 2>/dev/null); then
    src_name=$(nix eval --raw "$installable".src.name)
    src_rev=$(nix eval --raw "$installable".src.rev)
    src_fetch_submodules=$(nix eval "$installable".src.fetchSubmodules 2>/dev/null || echo "false")

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
    # shellcheck disable=SC2034
    unpackPhase="
runHook preUnpack
ln -s $NID_OUT $src_name

if [ -n \"${setSourceRoot:-}\" ]; then
  runOneHook setSourceRoot
fi

sourceRoot=${sourceRoot:-$src_name}
runHook postUnpack"
  fi
fi

# shellcheck source=/dev/null
. <(nix print-dev-env "$installable")
export NIX_LOG_FD=/dev/null
cd "$NIX_BUILD_TOP"
phases="${prePhases[*]:-} unpackPhase" genericBuild
if ! [ -L "$NIX_BUILD_TOP/${sourceRoot:-.}" ]; then
  cd "$(dirname "$NID_OUT")"
  mv -T "$NIX_BUILD_TOP/${sourceRoot:-.}" "$NID_OUT"
  ln -s "$NID_OUT" "$NIX_BUILD_TOP/${sourceRoot:-.}"
fi

cd "$NID_OUT"
cmakeFlags="-DCMAKE_EXPORT_COMPILE_COMMANDS=1 $cmakeFlags"
phases="patchPhase ${preConfigurePhases[*]:-} configurePhase" genericBuild

if [ -e compile_commands.json ]; then
  ln -rs compile_commands.json "$NIX_BUILD_TOP/$sourceRoot"
fi

rm -rf "$NIX_BUILD_TOP"
wait
