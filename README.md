nglint
======

A linter for nginx configuration files.

## Usage

````
nglint nginx.conf
````

`nglint` comes with two output formats. The default, pretty and a condensed
gcc-like format useful for integrating into other tools. To configure the
output format use the `-f` / `--format` flag.

````
nglint --format=gcc nginx.conf
````

## License

Code is licensed under MIT.

## Author

`nglint` was crafted with love by [Federico Bond](https://github.com/federicobond).
