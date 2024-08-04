installable="$1"

if git_url=$(nix eval --raw "$installable".src.gitRepoUrl 2>/dev/null); then
  rev=$(nix eval --raw "$installable".src.rev)
  dir="$(readlink -f "$(basename "$git_url" .git)")"

  # Shallow clone & checkout specific rev
  mkdir "$dir"
  cd "$dir"
  git init
  git remote add origin "$git_url"
  git fetch --depth 1 origin "$rev"
  git reset --hard FETCH_HEAD

  # Fetch submodules if necessary
  if [ "$(nix eval "$installable".src.fetchSubmodules 2>/dev/null)" == "true" ]; then
    git submodule update --init --recursive -j "$(nproc)" --progress --depth 1
  fi

  # Fetch all other commits in the background
  git fetch --all --unshallow --quiet &

  # shellcheck source=/dev/null
  . <(nix print-dev-env "$installable")

  # shellcheck disable=SC2034
  NIX_LOG_FD=/dev/null

  # shellcheck disable=SC2154
  sourceRoot="$(echo "$sourceRoot" | sed -E 's|^[^/]*/?||')"
  if [ -z "$sourceRoot" ]; then
    sourceRoot="."
  fi

  phases="${prePhases[*]:-} " genericBuild
  runHook postUnpack
  cd "$sourceRoot"
else
  # shellcheck source=/dev/null
  . <(nix print-dev-env "$installable")

  # shellcheck disable=SC2034
  NIX_LOG_FD=/dev/null

  # shellcheck disable=SC2154
  dir=$(readlink -f "${name%%-env}")

  cd "$NIX_BUILD_TOP"
  phases="${prePhases[*]:-} unpackPhase" genericBuild

  # shellcheck disable=SC2154
  mv "$NIX_BUILD_TOP/$sourceRoot" "$dir"
  cd "$dir"
fi

cmakeFlags="-DCMAKE_EXPORT_COMPILE_COMMANDS=1 $cmakeFlags"
phases="patchPhase ${preConfigurePhases[*]:-} configurePhase" genericBuild

if [ -e compile_commands.json ]; then
  ln -rs compile_commands.json "$dir"
fi

wait
