{ pkgs, modulesPath, ... }:

{
  imports = [
    "${toString modulesPath}/virtualisation/qemu-vm.nix"
    ./stateless.nix
  ];

  virtualisation = {
    memorySize = 1024 * 4;
    qemu.options = [
      "-smp $(${pkgs.coreutils}/bin/nproc)"
      "-device virtio-vga-gl"
      "-display gtk,gl=on,grab-on-hover=on"
    ];
  };
}
