# fift-asm-dsl

For creating multisig contract we chose to create a DSL for Fift
assembler language using Haskell.

Haskell has rich type system and we've found it perfectly capable
of handling types of a stack language Fift.

Our DSL allows user to implement a Fift contract. There are built-in
capabilities for:

 * composable serialization
 * checking current stack type in assert-like style
 * declaring intermediate procedures (even for polymorphic procedures)
 * introducing type aliases to prohibit occassionally confusing elements on stack
 * enhanced-type comparison operators (for not to confuse operands)

DSL nicely integrates with common Haskell syntax (one can use do-notaion and
if-then-else conditional statements).
After the contract is implemented in DSL, it can be printed into Fift
assembler and further be passed to `fift` compiler.

DSL was created for the purpose of this concrete task, thus only a subset
of fift assembler commands are covered. DSL has a lot of
potential for usability improvement:

* Automatic stack management (variable simulation)
* Support of all Fift assembler instructions
* Integration with QuickCheck for top-notch property-based testing

## Simple wallet contract

For convenience of the reader, simple wallet contract (clone of that from ton repository)
is re-implemnted in our DSL. Implementation is located in `wallet/Main.hs`,
generated code is located in `generated/wallet.fift`.

To run generator use command: `stack exec -- gen-wallet`.

## Multisig contract

Multisig contract is implemented according to requirements provided in contest description.

Implementation is located in `src/Multisig/` directory. File `src/Multisig/Impl.hs` contains
implementation of all methods (functions `recvExternal`, `recvInternal`, `getAllOrders`, `getOrdersByKey`, `getSeqno`).

Generated code can be accessed at `generated/multisig.fift`.

Storage of the contract contains:
  * Map from order id to public key set (for each order we keep who signed it already)
  * Map from order id to expiration timestamp
  * Nonce
  * K
  * Set of public keys (eligible for signing)

Many classes of bugs are eliminated by advanced typing. During contract development
many bugs were discovered and most of them were fixed with subsequent enhancing DSL
to eliminate such kind of bugs in future.

To run generator use command: `stack exec -- gen-multisig`.

## Usage instructions

Prerequisites:
1. Stack
2. Fift is installed and `FIFTPATH` points to the location of standard library files
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
1. First, you wrap the message into a multisig envelope. You can either:
    * Wrap an existing message using `wrap-msg` subcommand. In this case you provide
      * `<message_base>` — a file with the message excluding the `.boc` suffix,
      * `<expiration_timestamp>` — unix time (in seconds), when the order becomes invalid,
      * `<nonce>`.
    * Create a new message and wrap it into an envelop in one subcommand — `mk-msg`. In addition to envelope parameters you should also provide:
      * `<amount>` — the number of Grams you want to spend from the multisig contract,
      * `<dst_address>` — the destination smart contract,
      * (optionally) `<body_boc>` and `<init_boc>` — `.boc` files with custom `body` and `state_init` parameters.
2. After you wrapped the message into an envelope, you can sign the resulting `<message_base>.msig.boc` file with one of the private keys belonging to this multisig wallet. The envelope may contain multiple signatures, so you can send it off-chain to other signers if you wish. To sign the wrapped message, you need to invoke `sign-msg` subcommand, supplying:
    * `<message_base>` — base name without `.msig.boc` suffix,
    * `<multisig_addr>` — the address of the multisig contract (to prevent cross-contract replays),
    * `<private_key_file>` — the serialized private key file (as stored, e.g., by `mk-keypair`).
3. Once some number of signatures is collected, you should commit the signed message. To do this, you should invoke `commit-signed-msg` and provide:
    * `<message_base>` — base name without `.msig.boc` suffix,
    * `<multisig_addr>` — the address of the multisig contract.
4. You can now send the resulting `<message_base>.msig-query.boc` to the network using the client: `lite-client -C <config_path> --cmd "sendfile <message_base>.msig-query.boc"`. Once the contract receives the message, it will check the supplied signatures and either execute the message immediately, or put it on hold (to the orders dict) and wait for the sufficient quorum.
