# nixos-configuration
My nixos configuration

## Overiew
|-------------|-----------------------------------------------------------------------------------------------------------------------------|
| environment | Collection of features and packages that configure a specific environment.                                                  |
|             | Ex. `./environment/laptop.nix` installs WiFi and touchpad drivers.                                                          |
|-------------|-----------------------------------------------------------------------------------------------------------------------------|
| feature     | Single package or a collection of packages to perform a common action.                                                      |
|             | Ex. `./feature/text-editor.nix` will install my favourite text editor.                                                      |
|-------------|-----------------------------------------------------------------------------------------------------------------------------|
| package     | Custom configuration for a specific package.                                                                                |
|             | Ex. `./feature/emacs.nix` will install emacs with my custom configuration.                                                  |
|             |                                                                                                                             |
|             | If I don't configure a package, I will use `environment.systemPackages` instead of creating a seperate package file for it. |
|-------------|-----------------------------------------------------------------------------------------------------------------------------|
| users       | Configuration for users on my system                                                                                        |
|-------------|-----------------------------------------------------------------------------------------------------------------------------|

## Installation
Warning: I have not yet tested these instructions on a fresh install.

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
