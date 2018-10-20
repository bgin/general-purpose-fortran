          program demo_merge
          integer :: TSRC(2,3), FSRC(2,3), ANSWER(2,3)
          logical :: MASK(2,3)

          TSRC(1,:)=[ 1,6,5 ]; FSRC(1,:)=[ 0,3,2 ]; MASK(1,:)=[.TRUE., .FALSE.,.TRUE.]
          TSRC(2,:)=[ 2,4,6 ]; FSRC(2,:)=[ 7,4,8 ]; MASK(2,:)=[.FALSE.,.FALSE.,.TRUE.]

          ANSWER=MERGE(TSRC,FSRC,MASK)
          write(*,'(3i2)')(answer(i,:),i=1,size(answer,dim=1))
          end program demo_merge
