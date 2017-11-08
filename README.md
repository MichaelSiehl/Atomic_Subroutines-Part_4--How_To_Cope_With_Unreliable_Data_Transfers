# Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers
 Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 4: How to cope with unreliable data transfers with low-level PGAS programming- allow for safe remote communication among a number of coarray images.

# Overview
We use Fortran 2008 atomic subroutines to implement customized (or user-defined) synchronization procedures. However, remote data transfer through ATOMIC_DEFINE can be rather unreliable and unstable: even with the exactly same setup there is no guarantee that a remote data transfer will complete successfully with actual processors (OpenCoarrays/gfortran/OpenMPI on a Linux system). Yet, we can't even be sure that future implementations will offer stable runtime-behavior of remote transfer with ATOMIC_DEFINE since even the Fortran language standard leaves this processor-dependent.<br />
<br />
To overcome that problem, this GitHub repository contains a first implementation of a customized synchronization procedure that offers a synchronization diagnostics and allows for remote abort of a synchronization process from another coarray image (that itself is usually not involved with the regular synchronization process). Again, only few lines of Fortran code were required to accomplish that. <br />

# How it works
