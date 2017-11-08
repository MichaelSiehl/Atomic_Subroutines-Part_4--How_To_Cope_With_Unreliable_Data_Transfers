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
Here, the 'remote abort of synchronization status' is TRUE. Thus, the synchronization process was aborted by another coarray image (image 6). The ' number of successful remote synchronizations' is 0: none of the customized Event Post from the involved remote images did synchronize successfully with the customized Event Wait on image 1.<br />

3. The synchronization process does only partly complete successfully, some of the involved coarray images did fail to complete the synchronization process:
```fortran
involved remote images:                        2           3           4           0
 and the additional atomic values:              4           6           8           0
 Event-Wait+Scalar done on image:           1
 remote abort of synchronization (TRUE/FALSE): T
 remote image that did the abort:           6
 number of successful remote synchronizations:           3
 the successful image numbers:           3           2           4           0
```
Again, the 'remote abort of synchronization status' is TRUE. Thus, the synchronization process was aborted by another coarray image (image 6). The 'number of successful remote synchronizations' is 3: three of the involved remote images (2,3,4) did synchronize successfully with the customized Event Wait on image 1, one coarray image did fail to synchronize. The synchronization process (customized Event Post + customized Event Wait) must be repeated (a retry) only for the failed coarray image.<br />

# Main.f90: a simple test case
The test case should be compiled and run with 6 coarray images using OpenCoarrays/gfortran. Main.f90 contains the logic codes of this simple test case: on coarray image 1 we do execute the customized Event Wait to synchronize with a customized Event Post from coarray images 2,3,4,and 5; On coarray image 6 we do initiate (with a small time shift) a remote abort of the customized Event Wait on coarray image 1. (If necessary, we use one of the involved images 2,3,4,5 to do nothing on it so that the synchronization does (partly) fail). See the code of the test case in Main.f90:
```fortran
program Main
  ! a simple test-case for the customized Event Post / Event Wait
  ! (customized Event Wait with integrated synchronization abort and
  ! with integrated synchronization diagnostics)
  !
  use OOOGglob_Globals
  use OOOEerro_admError
  use OOOPimsc_admImageStatus_CA
  implicit none
  !
  integer(OOOGglob_kint) :: intNumberOfRemoteImages
  integer(OOOGglob_kint), dimension (1:4) :: intA_RemoteImageNumbers ! please compile and run this coarray
                                                                     ! program with 6 coarray images
  integer(OOOGglob_kint) :: intImageActivityFlag
  integer(OOOGglob_kint), dimension (1:4, 1:2) :: intA_RemoteImageAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intRemoteImageNumber
  integer(OOOGglob_kint) :: intEnumStepWidth
  integer(OOOGglob_kint) :: intAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCheckRemoteAbortOfSynchronization
  logical(OOOGglob_klog) :: logRemoteAbortOfSynchronization
  integer(OOOGglob_kint) :: intRemoteImageThatDidTheAbort
  integer(OOOGglob_kint) :: intNumberOfSuccessfulRemoteSynchronizations
  integer(OOOGglob_kint), dimension (1:4) :: intA_TheSuccessfulImageNumbers
  integer(OOOGglob_kint) :: intCount
  !
  !************************************************************************************************
  if (this_image() == 1) then ! do a customized Event Wait on image 1
    !
    intNumberOfRemoteImages = 4
    intA_RemoteImageNumbers = (/2,3,4,5/)
    !
    ! wait until all the involved remote image(s) do signal that they are in status InitiateASynchronization:
    intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitiateASynchronization
    intCheckRemoteAbortOfSynchronization = OOOPimscEnum_ImageActivityFlag % RemoteAbortOfSynchronization
    !
    ! spin-wait loop synchronization with emergency exit enabled (due to the intCheckRemoteAbortOfSynchronization
    ! argument):
    call OOOPimscEventWaitScalar_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, &
                intNumberOfRemoteImages, intA_RemoteImageNumbers, &
                intA_RemoteImageAndItsAdditionalAtomicValue = intA_RemoteImageAndItsAdditionalAtomicValue, &
                intCheckRemoteAbortOfSynchronization = intCheckRemoteAbortOfSynchronization, &
                logRemoteAbortOfSynchronization = logRemoteAbortOfSynchronization, &
                intRemoteImageThatDidTheAbort = intRemoteImageThatDidTheAbort, &
                intNumberOfSuccessfulRemoteSynchronizations = intNumberOfSuccessfulRemoteSynchronizations, &
                intA_TheSuccessfulImageNumbers = intA_TheSuccessfulImageNumbers)
    !
    write(*,*) 'invovled remote images:             ', intA_RemoteImageAndItsAdditionalAtomicValue(:,1)
    write(*,*) 'and the additional atomic values:   ', intA_RemoteImageAndItsAdditionalAtomicValue(:,2)
    write(*,*) 'Event-Wait+Scalar done on image:', this_image()
    write(*,*) 'remote abort of synchronization (TRUE/FALSE):', logRemoteAbortOfSynchronization
    write(*,*) 'remote image that did the abort:', intRemoteImageThatDidTheAbort
    write(*,*) 'number of successful remote synchronizations:', intNumberOfSuccessfulRemoteSynchronizations
    write(*,*) 'the successful image numbers:', intA_TheSuccessfulImageNumbers
  !************************************************************************************************
!  else if (this_image() == 4) then
    ! do nothing on image 4, so that the synchronization (customized Event Wait on image 1) must fail
  !************************************************************************************************
  else if (this_image() == 6) then ! image 6 is not involved with the synchronization itself,
                                   ! but only to abort the customized Event Wait synchronization on image 1:
    do intCount = 1, 99999999
      ! wait some time before doing the RemoteAbortOfSynchronization
    end do
    ! do abort the customized Event Wait on image 1:
    intRemoteImageNumber = 1
    intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % RemoteAbortOfSynchronization
    ! here, we use the customized Event Post to abort the synchronization from coarray image 6,
    ! which itself is not part of the synchronization:
    call OOOPimscEventPostScalar_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, &
                         intRemoteImageNumber, logExecuteSyncMemory = .true., &
                         intAdditionalAtomicValue = this_image())
  !************************************************************************************************
  else ! on all other images do a customized Event Post as part of the synchronization:
       ! (with current processors (OpenCoarrays/gfortran) it is highly possible that this may fail)
    intRemoteImageNumber = 1
    intImageActivityFlag = OOOPimscEnum_ImageActivityFlag % InitiateASynchronization
    intAdditionalAtomicValue = this_image() * 2 ! only a test case
    intEnumStepWidth = OOOPimscEnum_ImageActivityFlag % Enum_StepWidth ! only for error checking
    ! - signal to the remote image (image 1) that this image is now in state 'InitiateASynchronization':
    !
    call OOOPimscEventPostScalar_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, &
                         intRemoteImageNumber, intArrayIndex = this_image(), logExecuteSyncMemory = .true., &
                         intAdditionalAtomicValue = intAdditionalAtomicValue, intEnumStepWidth = intEnumStepWidth)
  !************************************
  end if
  !
!  write (*,*) 'execution finsished on image ', this_image()
  !
end program Main
```
