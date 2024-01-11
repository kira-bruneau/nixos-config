{ lib, config, pkgs, ... }:

let
  mkLiteral = value: {
    _type = "literal";
    inherit value;
  };

  mkValueString = value:
    if lib.isBool value then
      if value then "true" else "false"
    else if lib.isInt value then
      toString value
    else if (value._type or "") == "literal" then
      value.value
    else if lib.isString value then
      ''"${value}"''
    else if lib.isList value then
      "[ ${lib.concatStringsSep "," (map mkValueString value)} ]"
    else
      abort "Unhandled value type ${builtins.typeOf value}";

  mkKeyValue = { sep ? ": ", end ? ";" }:
    name: value:
      "${name}${sep}${mkValueString value}${end}";

  mkRasiSection = name: value:
    if lib.isAttrs value then
      let
        toRasiKeyValue = lib.generators.toKeyValue { mkKeyValue = mkKeyValue { }; };
        # Remove null values so the resulting config does not have empty lines
        configStr = toRasiKeyValue (lib.filterAttrs (_: v: v != null) value);
      in
      ''
        ${name} {
        ${configStr}}
      ''
    else
      (mkKeyValue
        {
          sep = " ";
          end = "";
        }
        name
        value) + "\n";

  toRasi = attrs:
    lib.concatStringsSep "\n" (lib.concatMap (lib.mapAttrsToList mkRasiSection) [
      (lib.filterAttrs (n: _: n == "@theme") attrs)
      (lib.filterAttrs (n: _: n == "@import") attrs)
      (removeAttrs attrs [ "@theme" "@import" ])
    ]);
in
{
  xdg.dataFile."rofi/themes/icon-grid.rasi".text = toRasi {
    configuration = {
      modi = "drun";
      show-icons = true;
      display-drun = "";
      drun-display-format = "{name}";
    };

    "*" = {
      font = "sans-serif, Font Awesome 6 Free Solid 10";
      background-color = mkLiteral "transparent";
      text-color = mkLiteral "#FFFFFFFF";
    };

    window = {
      background-color = mkLiteral "#2b303b99";
      transparency = "real";
      fullscreen = true;
    };

    mainbox = {
      padding = mkLiteral "100px 225px";
      spacing = mkLiteral "100px";
    };

    inputbar = {
      margin = mkLiteral "0% 25%";
      padding = mkLiteral "18px";
      spacing = mkLiteral "10px";
      border-radius = mkLiteral "10px";
      background-color = mkLiteral "white / 5%";
      children = [ "prompt" "entry" ];
    };

    entry = {
      placeholder = "Search";
    };

    listview = {
      columns = 6;
      cycle = false;
      fixed-columns = true;
      flow = mkLiteral "horizontal";
    };

    element = {
      border-radius = mkLiteral "10px";
      padding = mkLiteral "35px 10px";
      orientation = mkLiteral "vertical";
      spacing = mkLiteral "15px";
    };

    "element.selected.normal" = {
      background-color = mkLiteral "white / 5%";
    };

    element-icon = {
      size = mkLiteral "72px";
    };

    element-text = {
      horizontal-align = mkLiteral "0.5";
    };

    error-message = {
      padding = mkLiteral "100px";
      background-color = mkLiteral "black / 10%";
    };
  };

  home.packages = with pkgs; [
    font-awesome_6
  ];
}
