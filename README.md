# nixos-configuration

My NixOS configuration

## Overiew

| Directory                            | Description                                                                           |
|--------------------------------------|---------------------------------------------------------------------------------------|
| `cachix`<br><br>`cachix.nix`         | Binary cache configuration generated by `cachix use`                                  |
| `environment`                        | Collection of services and packages that configure some reusable environment          |
| `group`                              | Configuration for groups on my system                                                 |
| `hardware`                           | Generic hardware configuration that would be useless in the context of a VM           |
| `home`                               | [Home manager](https://github.com/nix-community/home-manager) configuration           |
| `host`                               | Machine specific configuration                                                        |
| `host/<name>/hardware`               | Machine specific hardware configuration that would be useless in the context of a VM  |
| `host/<name>/hardware/generated.nix` | Machine specific hardware configuration generated with `nixos-generate-config`        |
| `service`                            | Configuration for self-contained services                                             |
| `user`                               | Configuration for users on my system                                                  |
