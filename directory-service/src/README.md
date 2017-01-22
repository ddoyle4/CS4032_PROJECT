

##Caching
The directory service maintains a cache of files that are most often requested by clients.

The client makes the following gaurantees to the directory server:
  1. The client will obtain a lock on a file it is intending to write to
  2. If the client requests to write to a file, it will write to a file and so the file version on the primary server will be incremented by 1.

The cache replacement policy implemented here is Least Recently Used (LRU). This is implemented as follows:

* An **age** value is kept for each cache entry.
* When removing an entry to make space, the **oldest** value is removed first. That is, the entry with the **smallest** age value.
* When adding an entry, if it exists already, its age value is incremented. If it doesn't exist, it is assigned the **youngest** age value, or the max value for all current age values.


The current cache implementation behaves differently depending on whether the client intends to read/write a file and whether or not the file is currently saved in cache. Each of these situations are explained here:

* **Reading a file** - client requests to read a file
  * **Cache contains a copy of the file**
  The age value of the cache entry for the file is incremented and the copy is returned to the client.
  
  * **Cache doesn't contain a copy of the file**
  A new **empty** cache entry is stored in cache listing (the LRU cache entry is removed if necessary to make space). This new listing will record the file version for the file that the directory server has in the file's primary server record. A server record  (can be either primary or secondary record **depending on current file server loads**) specifying where this file is located is immediately returned to the client. At a later time (mo more than 5 seconds later) the directorie's task scheduler will contact the same file server an get a copy of this file and store it in the newly created cache entry.

* **Writing a file** - client requests to write to a file
  * **Cache contains a copy of the file**
  The **primary server** record (either existing alread or newly created) is immediately returned to the client and the cache entry is marked as "**dirty**". The current file version for this file is incremented from the older version O to the new version N in the cache and in the primary file server record. The task scheduler will then retrieve this file from the server pointed to by the primary server record. If the file version at that server is not equal to B the task scheduler will retry every 5 seconds until it retrives the correct version.

  * **Cache doesn't contain a copy of the file**
  The **primary server** record (either existing alread or newly created) is immediately returned to the client and a new cache entry is created with the requested file version + 1.

**Note**: If a client requests to write to a file that exists in the cache and has its **dirty** flag set it is assumed that a client wants to write to a file that has just been written to but has not been reflected in the cache yet. It is safe to assume that this client's update comes after the update that set the dirty flag in the first place due to the locking guarantees described above. In this situation, the safest thing to do is to increment the file version that the cache should expect when retrieving it from the file server. This can happen safely any number of times within the 5 second gap it takes the task scheduler to contact the file server.

**Note**: The primary server record will always have the most up-to-date information about a file, including the current file version. The secondary server records may be slightly behind but will be updated as file modifications propogate through the system.
