!Aditya Barman, Graduate student, Weizmann Institute of Science
!Date: May 29, 2025
!Email: atomicadi2023@gmail.com
program generate_cfour_input
       implicit none
       integer :: ios, nrows, i, j, k, num_file, unit_in, unit_out, pos, unit_line
          real, allocatable :: coor_mat(:,:)
     character(len=256) :: line, filename, output_file, output, cmd, trimmed_line
      
       nrows = 0
       num_file = 0
       unit_in = 20
       unit_out = 30
       call system("ls *.xyz > list.txt")
       open(unit=10, file='list.txt')
       do
         read(10, '(A)', IOSTAT=ios) line
         if (ios /= 0) exit
         num_file = num_file + 1
       end do
       close(unit=10)
       open(unit=10, file='list.txt')
       do i = 1, num_file
           read(10, '(A)') filename
           pos = index(filename, '.xyz')
           output = filename(1:pos-1) // '.zmat'
            open(unit=unit_in, file=filename)
            read(unit_in, *) 
            read(unit_in, *)
            do 
             read(unit_in, '(A)', IOSTAT=ios) line
             if (ios /= 0) exit
             nrows = nrows + 1
           end do
           allocate(coor_mat(nrows,4))
           close(unit=unit_in)
           open(unit=unit_in, file=filename)
           read(unit_in,*)
           read(unit_in,*)
           do j = 1, nrows
              read(unit_in,100) (coor_mat(j,k), k=1,4)
              100 format(2x, a1, 3x, 3F13.9)
           end do
           open(unit= unit_out, file=output)
           write(unit_out,'(A)') "Jobfile"
           do j = 1, nrows
              write(unit_out, '(a1,3x,F13.9,2x,F13.9,2x,F13.9)') coor_mat(j,1), coor_mat(j,2), coor_mat(j,3)
           end do
           write(unit_out,'(A)')
           write(unit_out,'(A)') "*CFOUR(CALC=CCSDT(Q),BASIS=SPECIAL,SPHERICAL=ON"
           write(unit_out,'(A)') "CHARGE=0"
           write(unit_out,'(A)') "MULTI=1"
           write(unit_out,'(A)') "CC_PROG=NCC"
           write(unit_out,'(A)') "SCF_CONV=9"
           write(unit_out,'(A)') "CC_CONV=8"
           write(unit_out,'(A)') "LINEQ_CONV=10"
           write(unit_out,'(A)') "FROZEN=ON"
           write(unit_out,'(A)') "COORD=CARTESIAN"
           write(unit_out,'(A)') "MEMORY=160,MEM_UNIT=GB)"
           write(unit_out,'(A)')
           do j = 1, nrows
              write(unit_out, '(a1)') coor_mat(j,1)
           end do
           write(unit_out,*)
           close(unit=unit_in)
           close(unit=unit_out)
           deallocate(coor_mat)
           nrows = 0
       end do
       close(unit=10)
       call system("rm list.txt")
       open(unit=50, file="A.sh")
       write(50, '(A)') 'for file in *.zmat; do'
       write(50, '(A)') '  awk ''{'
       write(50, '(A)') '    if ($0 == "H") print "H:cc-pVDZnoPonH";'
       write(50, '(A)') '    else if ($0 == "C") print "C:cc-pVDZ";'
       write(50, '(A)') '    else if ($0 == "N") print "N:cc-pVDZ";'
       write(50, '(A)') '    else if ($0 == "O") print "O:cc-pVDZ";'
       write(50, '(A)') '    else print $0;'
       write(50, '(A)') '  }'' "$file" > tmpfile && mv tmpfile "$file"'
       write(50, '(A)') 'done'
       close(unit=50)
       call system("chmod +x A.sh")
       call system("./A.sh")
       call system("rm A.sh")
end program generate_cfour_input
