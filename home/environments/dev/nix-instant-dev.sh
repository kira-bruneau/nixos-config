set -eo pipefail

installable=$1

NID_OUT=$(readlink -f "$(nix eval --raw "$installable".pname 2>/dev/null || nix eval --raw "$installable".name)")
mkdir "$NID_OUT"

while read -r line; do
  # shellcheck disable=SC2163
  export "$line"
done < <(nix eval --json --read-only "$installable" --apply 'p: {
  unpackPhase = p.unpackPhase or "";
  src_name = p.src.name;
  src_git_url = p.src.gitRepoUrl or "";
  src_rev = p.src.rev or "";
  src_fetch_submodules = p.src.fetchSubmodules or false;
}' | jq -r "to_entries|map(\"\(.key)=\(.value|tostring)\")|.[]")

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
cmakeFlags="-Cnix-instant-dev.cmake $cmakeFlags"
phases="patchPhase ${preConfigurePhases[*]:-} configurePhase" genericBuild

if [ -e compile_commands.json ]; then
  ln -rs compile_commands.json "$NIX_BUILD_TOP/$sourceRoot"
fi

rm -rf "$NIX_BUILD_TOP"
wait
