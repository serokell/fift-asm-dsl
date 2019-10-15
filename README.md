# fift-asm-dsl

## Multisig contract

## Usage instructions

Prerequisites:
1. Stack ???
2. Fift is installed and `FIFTPATH` points to the location of standard library
files.
3. TON lite-client is installed

Contract build:
1. `stack build`
2. `mkdir build`
3. `stack exec fift-asm-dsl-exe > build/multisig.fif`

Once you've build the contract, you can prepare and sign messages using the
provided CLI. The CLI is invoked by calling
`fift -s scripts/Main.fif <subcommand> <args...>`. If invoked with no arguments,
the CLI prints usage instructions and exits.

Deployment workflow:
1. Generate private and public keys for parties using the `mk-keypair` CLI subcommand.
2. Use CLI subcommand `new` to deploy a new multisig contract. This subcommand
generates an `.addr` file containing the multisig address, and a `.boc` file
with an external deployment message. Use `build/multisig.fif` as the first
subcommand argument to point to the contract code built on the previous stage. The `new` subcommand accepts the following parameters:
    * `<multisig_contract_file>`
    * `<workchain_id>`
    * `<outfile_base>` — base name for query and address files,
    * `<K>` — minimum number of signatures for quorum,
    * `<pk1>, <pk2>, ..., <pkN>` — files with serialized public keys.
3. Fuel the contract by sending some Grams to the provided address using a
non-bounceable transaction.
4. Send the message to the network using
`lite-client -C <config_path> --cmd "sendfile <outfile_base>-query.boc"`

The deployed multisig contract sends some (internal) _message_ if and only if the quorum is reached.
