          program demo_exchange
          use M_sort, only : exchange
          integer             :: iarray(2)=[10,20]
          real                :: rarray(2)=[11.11,22.22]
          doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
          complex             :: carray(2)=[(1234,56789),(9876,54321)]
          logical             :: larray(2)=[.true.,.false.]
          character(len=16)   :: string(2)=["First string    ","The other string"]

          integer             :: one(13)=1
          integer             :: two(13)=2

          integer             :: one2(3,3)=1
          integer             :: two2(3,3)=2

             print *, "integers before exchange ", iarray
             call exchange (iarray(1), iarray(2))
             print *, "integers after exchange  ", iarray

             print *, "reals before exchange ", rarray
             call exchange (rarray(1), rarray(2))
             print *, "reals after exchange  ", rarray

             print *, "doubles before exchange ", darray
             call exchange (darray(1), darray(2))
             print *, "doubles after exchange  ", darray

             print *, "complexes before exchange ", carray
             call exchange (carray(1), carray(2))
             print *, "complexes after exchange  ", carray

             print *, "logicals before exchange ", larray
             call exchange (larray(1), larray(2))
             print *, "logicals after exchange  ", larray

             write(*,*)'GETS THIS WRONG IN GFORTRAN'
             print *, "strings before exchange ", string
             call exchange (string(1), string(2))
             print *, "strings after exchange ", string

             write(*,*)'exchange two vectors'
             write(*,'("one before: ",*(i0,:","))') one
             write(*,'("two before: ",*(i0,:","))') two
             call exchange(one,two)
             write(*,'("one after: ",*(i0,:","))') one
             write(*,'("two after: ",*(i0,:","))') two

             write(*,*)'given these arrays initially each time '
             one2=1
             two2=2
             call printarrays()

             write(*,*)'GETS THIS WRONG'
             write(*,*)'exchange two rows'
             one2=1
             two2=2
             call exchange(one2(2,:),two2(3,:))
             call printarrays()

             write(*,*)'GETS THIS WRONG'
             write(*,*)'exchange two columns'
             one2=1
             two2=2
             call exchange(one2(:,2),two2(:,2))
             call printarrays()

             write(*,*)'CANNOT DO MULTI-DIMENSIONAL ARRAYS YET'
             write(*,*)'exchange two arrays with same number of elements'
             one2=1
             two2=2
             !call exchange(one2,two2)
             !call printarrays()

             contains
             subroutine printarrays()
             integer :: i
             do i=1,size(one2(1,:))
                write(*,'(*(i0,:","))') one2(i,:)
             enddo
             write(*,*)
             do i=1,size(two2(1,:))
                write(*,'(*(i0,:","))') two2(i,:)
             enddo
             end subroutine printarrays

              end program demo_exchange
