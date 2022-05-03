# nixos-configuration
My NixOS configuration

## Overiew
| Directory   | Description                                                                                                                                                                                                                                                   |
|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| environment | Collection of features and packages that configure a specific environment.<br>Ex. `./environment/laptop.nix` installs WiFi and touchpad drivers.                                                                                                              |
| user        | Configuration for users on my system                                                                                                                                                                                                                          |

## Installation
See the [NixOS Manual](https://nixos.org/nixos/manual/index.html#ch-installation)
for detailed installation instructions.

Before running `nixos-generate-config`, clone this repo to the nixos configuration directory.

For example, if you are following the NixOS manual:
```shell
nix-env -i git
git clone https://gitlab.klocwork.com/kbruneau/nixos-configuration.git /mnt/nixos
nixos-generate-config --root /mnt
```

Modify the generated `configuration.nix` file to import the
environments and users that you want to install and then run:
```shell
nixos-install
```
