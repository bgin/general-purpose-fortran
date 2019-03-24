           program demo_size
           integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
              write(*,*) 'SIZE of simple one-dimensional array=',size([ 11, 22, 33 ])    ! 3

              write(*,*)'body'
              write(*,*)'SHAPE(arr)       :',shape(arr)
              write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
              write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is not "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
              write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

              call interfaced(arr,arr)
              call nointerface(arr)
           contains

           subroutine interfaced(arr,arr2)
           integer,intent(in)  :: arr(:,:)
           integer,intent(in)  :: arr2(2,*)
              write(*,*)'interfaced assumed-shape array'
              write(*,*)'SHAPE(arr)       :',shape(arr)
              write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
              write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
              write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
              write(*,*)'interfaced'
              write(*,*)'SHAPE(arr)       :',shape(arr)
              write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
              write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
              write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
           end subroutine interfaced
           !!
           ! NOTE: If NOINTERFACE(3f) had an assumed-shape argument with : for dimensions it
           !       could only be properly called with an explicit interface
           !!
           subroutine nointerface(arr)
           integer,intent(in) :: arr(3,*)
              write(*,*)'nointerface'
            !!write(*,*)'SHAPE(arr)       :',shape(arr)  !! SHAPE(3f) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
            !!write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
            !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2) !! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
            !!write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
            !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
           end subroutine nointerface
           !!
           end program demo_size
