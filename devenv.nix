{ pkgs, lib, config, inputs, ... }:
{
  packages = [  ];

  languages.scala = {
    enable = true;
  };

  enterShell = ''
    echo "~~~ tttm ~~~"
    type javac && type scalac && type python
    echo "JAVA_HOME=$JAVA_HOME"
  '';

  enterTest = ''
    echo "Running tests"
  '';

}
