# Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers
 Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 4: How to cope with unreliable data transfers with low-level PGAS programming- allow for safe remote communication among a number of coarray images.

# Overview
We use Fortran 2008 atomic subroutines to implement customized (or user-defined) synchronization procedures. However, remote data transfer through ATOMIC_DEFINE can be rather unreliable and unstable: even with the exactly same setup there is no guarantee that a remote data transfer will complete successfully with actual processors (OpenCoarrays/gfortran/OpenMPI on a Linux system). Yet, we can't even be sure that future implementations will offer stable runtime-behavior of remote transfer with ATOMIC_DEFINE since even the Fortran language standard does seem to leave this processor-dependent.<br />
<br />
To overcome that problem, this GitHub repository contains a first implementation of a customized synchronization procedure that offers a synchronization diagnostics and allows for remote abort of a synchronization process from another coarray image (that itself is usually not involved with the regular synchronization process). Again, only few lines of Fortran code were required to accomplish that. <br />

# How it works
The code implements customized (or user-defined) Event Post and Event Wait procedures: EventPostScalar and EventWaitScalar do have functionality that is similar to Fortran 2015 Events with the addition that the customized version does allow to transfer additional scalar data together with the synchronization. Further, the customized version does offer a synchronization diagnostic, even if the synchronization process gets aborted remotely (from another coarray image).
See the following outputs from distinct program runs:<br />


1. The synchronization process as a whole does complete successfully:
```fortran
 involved remote images:                        2           3           4           5
 and the additional atomic values:              4           6           8          10
 Event-Wait+Scalar done on image:           1
 remote abort of synchronization (TRUE/FALSE): F
 remote image that did the abort:           0
 number of successful remote synchronizations:           0
 the successful image numbers:           3           4           5           2

```
Here, the 'remote abort of synchronization status' is FALSE. Thus, the synchronization process as a whole did complete successfully before the synchronization abort was initiated remotely.<br />

2. The synchronization process as a whole does fail completely :
```fortran
 involved remote images:                        0           0           0           0
 and the additional atomic values:              0           0           0           0
 Event-Wait+Scalar done on image:           1
 remote abort of synchronization (TRUE/FALSE): T
 remote image that did the abort:           6
 number of successful remote synchronizations:           0
 the successful image numbers:           0           0           0           0
```
Here, the 'remote abort of synchronization status' is TRUE. Thus, the synchronization process was aborted by another coarray image (image 6). The ' number of successful remote synchronizations' is 0: none of the Event Post from the involved remote images did synchronize successfully with the customized Event Wait on image 1.<br />


