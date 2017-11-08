! https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers
!
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











!! https://github.com/MichaelSiehl/Atomic_Subroutines-Part_3b--How_To_Process_Array_Data
!
!program Main
!  use OOOGglob_Globals
!  use OOOEerro_admError
!  use OOOPimsc_admImageStatus_CA
!  implicit none
!  !
!  integer(OOOGglob_kint) :: intNumberOfRemoteImages
!  integer(OOOGglob_kint), dimension (1:3) :: intA_RemoteImageNumbers ! please compile and run the
!                                                                     ! program with 4 coarray images
!  integer(OOOGglob_kint) :: intSetFromImageNumber
!  integer(OOOGglob_kint), dimension (1:5) :: intA_TestArrayForRemoteTransfer
!  integer(OOOGglob_kint), dimension (1:3,1:5) :: intA_TestArrayForRemoteReceive
!  integer(OOOGglob_kint) :: intTestArrayUpperBound
!  integer(OOOGglob_kint) :: intCount
!  integer(OOOGglob_kint) :: intA
!  !
!  intTestArrayUpperBound = ubound(intA_TestArrayForRemoteTransfer, 1)
!  !
!  !********************************* execute on coarray image 1:
!  !
!  if (this_image() == 1) then
!    intNumberOfRemoteImages = 3
!    intA_RemoteImageNumbers = (/2,3,4/)
!    !
!    ! initiate and wait for remote transfer of the TestArray data:
!    call OOOPimsc_InitiateAndWaitForTestArrayTransfer_CA (OOOPimscImageStatus_CA_1, intTestArrayUpperBound, &
!                      intA_TestArrayForRemoteReceive, intNumberOfRemoteImages, intA_RemoteImageNumbers)
!    !
!    do intCount = 1, intNumberOfRemoteImages
!      write(*,*) 'remote array transfer done: on image / array data', &
!                             this_image(), '/', intA_TestArrayForRemoteReceive(intCount,:)
!    end do
!    !
!  !*********************************** execute on coarray image 2,3,and 4:
!  else ! this_image > 1
!    intNumberOfRemoteImages = 1
!    intA_RemoteImageNumbers(1) = 1
!    intA = this_image()
!    intA_TestArrayForRemoteTransfer = (/intA,intA,intA,intA,intA/) ! fill the TestArray with some data locally
!    !
!    ! synchronize and distribute the TestArray data to the remote coarray image 1:
!    call OOOPimsc_SynchronizeAndDistributeTheTestArray_CA (OOOPimscImageStatus_CA_1, &
!            intNumberOfRemoteImages, intA_RemoteImageNumbers, intTestArrayUpperBound, intA_TestArrayForRemoteTransfer)
!    !
!  !************************************
!  end if
!  !
!end program Main
