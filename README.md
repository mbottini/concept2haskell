# concept2haskell

Haskell parser of the Concept2 PM5's binary format.

# Haskell Dependencies

* `Aeson`
* `Aeson.Pretty`

On Debian, you can get them with 

    sudo apt install libghc-aeson-dev libghc-aeson-pretty-dev

On Arch:

    sudo pacman -S haskell-aeson haskell-aeson-pretty

# Usage

Compile with `make`.

To run, copy the following files from your Concept2 USB stick to the
working directory:

* LogDataAccessTbl.bin
* LogDataStorage.bin

Run with `./ergparse`.