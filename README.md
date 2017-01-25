# CS4032 Distributed File System
This repository, including the API repository located [here] here, consitutes my submission for the CS4032 project.
All of the features of the distributed system have been implemented and are described below.

## Setting Up
Each service resides in a separate directory under root. Each service can be built and run with the following:
```sh
$ stack build
$ stack image container
$ docker-compose up
```
Each service should be set up and running before interacting with the system. The user should then run the `configure` command, as detailed below, to begin a guided set up.

## Interaction
The user interacts with the system using the **my-client**, with the following general command line argument structure:
```sh
$ stack exec client-exe <argument>
```
where *<argument>* can be the following:
- `configure` - launches a guided set up of system - should be ran first
- `clean-etc` - removes all of the local configuration files (saved server addresses, etc.)
- `add-user <name> <password>` - adds a user to the authentification database
- `auth <name> <password>` - authenticates the user and saves a token that will be used to encrypt data sent to the system. This must be executed before files can be written/read
- `add-file-server <host> <port> <unique-identifier>` - adds a new file server to the system that will immediately be used for storing files. The unique identifier must match the identifier that was built with the file server instance.
- `write-file <filename>` - name of the file to store in the system
- `read-file <filename>` - name of the file to retrieve from the system

## File System Components

### Replication
File Servers are aware of each other. They will constantly try to push files that have not been replicated a specified number of times to other File Servers (based on current server loads and free space). Once the files are replicated other servers, the serverd will inform the Directory Server that they have a secondary copy available. Any writes to a file will be performed on the primary copy, and these changes will be propagated to the servers containing the secondary copies automatically during periodic updates.
### Transactions
Transactions are implemented by the Transaction Server. The user can initiate a transaction with this server. The server will act as a proxy for the user from this point forward. The server writes changes to **shadow files** on the File Server containing the primary copy of the file. When the user commits the transaction, the server will inform the appropriate file servers to push the shadow file into the main copy of the primary file. Alternatively, aborts will prompt the server to advise all File Servers to dump the relevant shadow files.
### Directory
The Directory Server contains records for all primary and secondary files in the system. The client will resolve file locations here to determine where to read/write files. The directory server also maintains a list of all of the servers active in the system and informs every server of the presence of every other server. Caching and replication are also facilitated here as described in the other sections
### Locking
The Lock Server provides a simple locking implementation. The client, or the transaction server, can lock/unlock any particular file to guarantee integrity.
### Security
The Authentification Server implements security protocols to ensure privacy on the system's network. It implements a version of Kerberos authentification protocol. The users sends an autherisation request to the server. The server responds with a package encrypted with their password. The user decrypts this package to produce a key (Key1) to encrypt messages it sends into the system and a token. This token is encrypted with a key (Key2) known only to the system and contains the key used by the client (Key1) and metadata. This facilitates secure communication within the system
### Caching
Caching is implemented at the directory server with a Least Recently Used (Cache Replacement Policy). There is a chance of files being in the cache when clients attempt to resolve a files location, which saves the client connecting to the primary file server for the file. Cache integrity is preserved through writes.

[here]: https://github.com/ddoyle4/file-system-api.git

