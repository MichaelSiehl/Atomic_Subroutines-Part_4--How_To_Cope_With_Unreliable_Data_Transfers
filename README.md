# Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers
 Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 4: How to cope with unreliable data transfers with low-level PGAS programming- allow for safe remote communication among a number of coarray images.

# Overview
We use Fortran 2008 atomic subroutines to implement customized (or user-defined) synchronization procedures. However, remote data transfer may fail (because of a coding error for an example).<br />
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
The test case should be compiled and run with 6 coarray images using OpenCoarrays/gfortran or ifort 18 uodate 1. Main.f90 contains the logic codes of this simple test case: on coarray image 1 we do execute the customized Event Wait to synchronize with a customized Event Post from coarray images 2,3,4,and 5; On coarray image 6 we do initiate (with a small time shift) a remote abort of the customized Event Wait on coarray image 1. (If necessary, we use one of the involved images 2,3,4,5 to do nothing on it so that the synchronization does (partly) fail). See the code of the test case in Main.f90:
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
# The customized Event Wait (EventWaitScalar)
The customized Event Wait synchronization procedure (EventWaitScalar), in the OOOPimsc_admImageStatus_CA.f90 source code file, does contain a kind of emergency exit: the functionality to check for a synchronization abort through a customized Event Post from another coarray image. The EventWaitScalar does also provide the synchronization diagnostics through its optional arguments. Only few lines of Fortran code were required to implement this:
```fortran
subroutine OOOPimscEventWaitScalar_intImageActivityFlag99_CA (Object_CA, intCheckImageActivityFlag, &
                  intNumberOfImages, intA_RemoteImageNumbers, logArrayIndexIsThisImage, &
                  intA_RemoteImageAndItsAdditionalAtomicValue, logExecuteSyncMemory, &
                  intCheckRemoteAbortOfSynchronization, logRemoteAbortOfSynchronization, intRemoteImageThatDidTheAbort, &
                  intNumberOfSuccessfulRemoteSynchronizations, intA_TheSuccessfulImageNumbers)
  ! customized Event Wait plus Scalar:
  ! (An optional (limited size integer) scalar value will be unpacked in the intA_RemoteImageAndItsAdditionalAtomicValue(:,2)
  ! optional output argument)
  !
  ! This routine is for atomic bulk synchronization (among the executing image and one or more remote images)
  ! using a spin-wait loop synchronizaton. Thus, the procedure implements a customized synchronization
  ! using atomic subroutines and the sync memory statement internally. Ordered execution segments among the involved images
  ! are not required.
  !
  ! Remote data transfer through atomic_define may be unreliable. Thus, this procedure implements a way to abort the
  ! synchronization from another coarray image (that is usually not involved with the synchronization itself),
  ! through the intCheckRemoteAbortOfSynchronization argument. Further, and in case of such a synchronization abort,
  ! this routine gives a synchronization diagnostic (through its logRemoteAbortOfSynchronization, intRemoteImageThatDidTheAbort,
  ! intNumberOfSuccessfulRemoteSynchronizations, and intA_TheSuccessfulImageNumbers optional arguments).
  !
  type (OOOPimsc_adtImageStatus_CA), codimension[*], intent (inout) :: Object_CA
  integer(OOOGglob_kint), intent (in) :: intCheckImageActivityFlag
  integer(OOOGglob_kint), intent (in) :: intNumberOfImages ! these are the number of involved remote images
  integer(OOOGglob_kint), dimension (1:intNumberOfImages), intent (in) :: intA_RemoteImageNumbers
  logical(OOOGglob_klog), optional, intent (in) :: logArrayIndexIsThisImage
  logical(OOOGglob_klog) :: logArrIndexIsThisImage
  integer(OOOGglob_kint) :: intArrIndex
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfImages, 1:2), intent (out) :: &
                                                       intA_RemoteImageAndItsAdditionalAtomicValue
  integer(OOOGglob_kint) :: intCount
  integer(OOOGglob_kint) :: intImageNumber
  logical(OOOGglob_klog), dimension (1:intNumberOfImages) :: logA_CheckImageStates
  integer(OOOGglob_kint) :: intAtomicValue = 0
  logical(OOOGglob_klog), optional, intent (in) :: logExecuteSyncMemory
  logical(OOOGglob_klog) :: logSyncMemoryExecution
  integer(OOOGglob_kint) :: intMaxVal
  !
  integer(OOOGglob_kint), optional, intent (in) :: intCheckRemoteAbortOfSynchronization
  logical(OOOGglob_klog), optional, intent (out) :: logRemoteAbortOfSynchronization
  integer(OOOGglob_kint), optional, intent (out) :: intRemoteImageThatDidTheAbort
  integer(OOOGglob_kint), optional, intent (out) :: intNumberOfSuccessfulRemoteSynchronizations
  integer(OOOGglob_kint), optional, dimension (1:intNumberOfImages), intent (out) :: intA_TheSuccessfulImageNumbers
  integer(OOOGglob_kint) :: intCurrentPosition
  !
  integer(OOOGglob_kint) :: status = 0 ! error status
  !
                                                                call OOOGglob_subSetProcedures &
                                            ("OOOPimscEventWaitScalar_intImageActivityFlag99_CA")
  !
  !**********************************************************************
  !
  if (present(intA_RemoteImageAndItsAdditionalAtomicValue)) &
                              intA_RemoteImageAndItsAdditionalAtomicValue(:,:) = 0 ! initial value
  !
  if (present(intA_TheSuccessfulImageNumbers)) intA_TheSuccessfulImageNumbers = 0 ! initial value
  !****
  if (present(logArrayIndexIsThisImage)) then
    logArrIndexIsThisImage = logArrayIndexIsThisImage
  else ! default:
    logArrIndexIsThisImage = .false.
  end if
  !****
  if (present(logExecuteSyncMemory)) then
    logSyncMemoryExecution = logExecuteSyncMemory
  else ! default:
    logSyncMemoryExecution = .true.
  end if
  !****
  !
  if (present(logRemoteAbortOfSynchronization)) then
    logRemoteAbortOfSynchronization = .false. ! initial value
  end if
  !****
  if (present(intRemoteImageThatDidTheAbort)) then
    intRemoteImageThatDidTheAbort = 0 ! initial value
  end if
  !****
  if (present(intNumberOfSuccessfulRemoteSynchronizations)) then
    intNumberOfSuccessfulRemoteSynchronizations = 0 ! initial value
  end if
  !****
  ! initialize the array elements with .false.:
  logA_CheckImageStates = .false.
  !
  !**********************************************************************
  ! wait until all the involved remote image(s) do signal that they are in state intCheckImageActivityFlag
  ! spin-wait loop synchronization:
  do
    do intCount = 1, intNumberOfImages
      !
      intImageNumber = intA_RemoteImageNumbers(intCount)
      intArrIndex = intImageNumber ! but:
        if (logArrIndexIsThisImage) intArrIndex = this_image()
      if (intImageNumber .ne. this_image()) then ! (synchronization is only required between distinct images)
        if (.not. logA_CheckImageStates(intCount)) then ! check is only required if the remote image is not already
                                                        ! in state intCheckImageActivityFlag:
          !
          if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
                           intCheckImageActivityFlag, intArrayIndex = intArrIndex, &
                           intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
            logA_CheckImageStates(intCount) = .true. ! the remote image is in state intCheckImageActivityFlag
            !
            if (present(intA_RemoteImageAndItsAdditionalAtomicValue)) then
            ! save the remote image number together with its sent AdditionalAtomicValue:
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,1) = intImageNumber
              intA_RemoteImageAndItsAdditionalAtomicValue(intCount,2) = intAtomicValue
            end if
            !
            ! record the remote image numbers that did complete the synchronization successfully:
            if (present(intA_TheSuccessfulImageNumbers)) then
              ! count how many images completed the synchronization successfully so far:
              ! (to determine the current positon)
              intCurrentPosition = count(logA_CheckImageStates)
              ! save the successful remote image number:
              intA_TheSuccessfulImageNumbers(intCurrentPosition) = intImageNumber
            end if
          end if
        end if
      else ! (intImageNumber .eq. this_image())
        ! raise an error:
                                                                call IIimsc_ErrorHandler (Object_CA, &
                                                            "the executing image can't synchronize with itself yet", &
                                                                  OOOGglob_error, status)
                                                                !
        logA_CheckImageStates(intCount) = .true. ! otherwise the outer do loop would turn into an endless loop
      end if
    end do
    !
    if (all(logA_CheckImageStates)) then ! all involved remote images are in state intCheckImageActivityFlag
      if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
      exit ! exit the do loop if all involved remote images are in state
                                         ! intCheckImageActivityFlag
    end if
    !
    ! for leaving a fault synchronization do the following check:
    if (present(intCheckRemoteAbortOfSynchronization)) then
      if (OOOPimscGAElement_check_atomic_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, &
          intCheckImageActivityFlag = intCheckRemoteAbortOfSynchronization, intArrayIndex = this_image(), &
          intAdditionalAtomicValue = intAtomicValue, logExecuteSyncMemory = .false.)) then
        !
        if (logSyncMemoryExecution) call OOOPimsc_subSyncMemory (Object_CA) ! execute sync memory
        !
        if (present(logRemoteAbortOfSynchronization)) then
          logRemoteAbortOfSynchronization = .true.
        end if
        !
        if (present(intRemoteImageThatDidTheAbort)) then
          intRemoteImageThatDidTheAbort = intAtomicValue
        end if
        !
        if (present(intNumberOfSuccessfulRemoteSynchronizations)) then
          intNumberOfSuccessfulRemoteSynchronizations = count(logA_CheckImageStates)
        end if
        !
        exit ! exit the do loop if the status is 'intCheckRemoteAbortOfSynchronization'
             ! (i.e. a(nother) remote image does signal to abort the synchronization process)
      end if
    end if
    !
  end do
  !
  !**********************************************************************
  !
                                                                call OOOGglob_subResetProcedures
  !
end subroutine OOOPimscEventWaitScalar_intImageActivityFlag99_CA
```
