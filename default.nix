{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
{
  record1 = import ./record1 { inherit nixpkgs compiler; };
  record1-lens = import ./record1-lens { inherit nixpkgs compiler; };
}
