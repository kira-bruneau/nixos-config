{
  inputs,
  config,
  pkgs,
  ...
}:

let
  nixos-generate-config =
    (pkgs.writeShellApplication {
      name = "nixos-generate-config";

      runtimeInputs = with pkgs; [
        coreutils
        git
        config.system.build.nixos-generate-config
      ];

      text = ''
        root=
        dir=/etc/nixos
        force=0
        show_hardware_config=0

        while [ $# -gt 0 ]; do
          case $1 in
            --help) nixos-generate-config --help; exit $?;;
            --root) root="''${2%/}"; shift 2;;
            --dir) dir="$2"; shift 2;;
            --force) force=1; shift 1;;
            --show-hardware-config) show_hardware_config=1; shift 1;;
            *) echo "$0: unrecognized argument '$1'" >&2; exit 1;;
          esac
        done

        dir="''${root%/}$dir"

        if [ -n "$root" ]; then
          args=(--root "$root")
        else
          args=()
        fi

        args+=(
          --dir "$dir"
          --no-filesystems
          --show-hardware-config
        )

        if [ $show_hardware_config -ne 0 ]; then
          exec nixos-generate-config "''${args[@]}"
        fi

        if [ $force -ne 0 ]; then
          rm -rf "$dir"
        fi

        if [ -z "$(ls -A "$dir" 2>/dev/null)" ]; then
          mkdir -p "$dir"
          cp -RT --no-preserve=ownership ${inputs.self} "$dir"
          chmod -R +w "$dir"
          git -C "$dir" init --initial-branch main
          git -C "$dir" add --all
          git -C "$dir" config include.path ../.gitconfig
          fresh=1
        else
          echo "warning: not overwriting existing flake at $dir" >&2
          fresh=0
        fi

        out="$dir/hardware/hosts/$(hostname)/generated.nix"
        echo "writing $out..." >&2
        mkdir -p "$(dirname "$out")"
        nixos-generate-config "''${args[@]}" > "$out"
        git -C "$dir" add "$out"
        if [ $fresh -ne 0 ]; then
          git -C "$dir" -c user.name='Kira Bruneau' -c user.email='kira.bruneau@pm.me' commit -m 'nixos-generate-config'
        fi
      '';
    })
    // {
      meta = {
        priority = 1;
      };
    };
in
{
  environment.systemPackages = [ nixos-generate-config ];
}
