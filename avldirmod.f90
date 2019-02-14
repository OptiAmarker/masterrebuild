 module avldiskmod
  character,parameter:: gext*4=".DAT"
  integer igavlf,iavlinitdone,igavlc1,igavlc2
  character gavlfull*200,gavldir*100
  contains
  
!============================ OPENAVL
      subroutine openavl
      use fileutilmod
      igavlf=ifopen("AVLDISK"//gext,"R")
      iavlinitdone=777
      call columnfind
      return
      end subroutine
!============================ AVLDIR
      subroutine avldir(filein)
      use fileutilmod
      use othermod
      use strutilmod
      character sf1*8,sf2*8,tmpstr*12,filefind*8,filein*(*)

      i1st=igavlc1
      i2st=igavlc2

      ibad=0
      if(i1st==0.or.i1st>100)ibad=1
      if(i2st==0.or.i2st>100)ibad=1
      if(ibad==1)call enditall("You must call COULMNFIND/OPENAVL!")

      idebug=isdebugactive("avldir")

      call ucase(filein)

      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv   avldir"
       print *,":",filein,":"
       print *,"Column starts: ",igavlc1,igavlc2
      endif

      filefind=filein
      if(filefind(1:1)==" ")then
       print *, "You must specify a file to find."
       stop
      endif

      call stripext(filefind)
      if(idebug==1)print *,"filefind: ",filefind

      ifoundit=0

      tmpstr=filefind
      call ucase(tmpstr)
      if(tmpstr=="AVLDISK".or.tmpstr=="TRACE".or.tmpstr=="PRINT".or.tmpstr=="PLOT")then
       gavldir="./"
       goto 20
      endif

!      if(ifilchk("AVLDISK"//gext,"r")==0)then
!       gavldir="./"
!       print *,"AVLDISK.DAT not found, assuming ",trim(filein)//gext//" is local."
!       goto 20
!      endif
      call wholepos(igavlf,0)
!      iff=fopen("AVLDISK.DAT","i ")
      do
       call nextline(igavlf,ieof)
       if(ieof/=0)exit
!       print *,trim(fileline)
       gavldir=fileline(2:i1st-1)
       sf1=fileline(i1st:i1st+8)
       sf2=fileline(i2st:i2st+8)
       ic1=istrcomp(filefind,sf1,1)
       ic2=istrcomp(filefind,sf2,1)
!       print *,sdir,sf1,sf2
!       print *,ic1
!       print *,ic2
!       pause
       if(ic1==-1)goto 10
       if(ic2==1)goto 10
       if(ic2==0.and.ic1/=0)goto 10
! this is where it goes!!
       ifoundit=1
       exit
 10    continue
      enddo
      if(ifoundit==0)gavldir="./"
!      call fclose(iff)
 20   continue

      il=len_trim(gavldir)
      if(gavldir(il:il)/="/")gavldir=trim(gavldir)//"/"
      call ucase(gavldir)
      gavlfull=trim(gavldir)//trim(filefind)//trim(gext)

      if(idebug==1)then
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   avldir"
      endif

      return
      end subroutine
!========================= COLUMNFIND
      subroutine columnfind
      use fileutilmod
      use othermod

      if(iavlinitdone/=777)call enditall("You must call AVLOPEN!")

      call wholepos(igavlf,0)

      ilinecnt=0; icol1tot=0; icol2tot=0; icnt1=0; icnt2=0
      do
       call nextline(igavlf,ieof)
       if(ieof.ne.0)exit
       if(fileline(1:1)=="#")exit
       ilinecnt=ilinecnt+1
       ilen=len_trim(fileline)
!       print *,"ilen: ",ilen

       ist=2
! find space
       do
        if(fileline(ist:ist)==" ")exit
        ist=ist+1
       enddo
! find non-space, beginning of first column
       do
        if(fileline(ist:ist)/=" ")exit
        ist=ist+1
       enddo
       icol1tot=icol1tot+ist
       icnt1=icnt1+1
!       print *,ist
! find space
       do
        if(fileline(ist:ist)==" ")exit
        ist=ist+1
        if(ist>ilen)goto 10
       enddo
! find non-space, beginning of second column
       do
        if(fileline(ist:ist)/=" ")exit
        ist=ist+1
       enddo
       icol2tot=icol2tot+ist
       icnt2=icnt2+1
 10    continue
!       print *,icnt1,icol1tot
!       print *,icnt2,icol2tot
      enddo

      igavlc1=int((icol1tot/real(icnt1))+.5)
      igavlc2=int((icol2tot/real(icnt2))+.5)

      return
      end subroutine
! =============================== ISTRCOMP
      function istrcomp(stf,stavl,iallowwc)
! IALLOWWC   1 - allow a * wildcard
!            0 - * characters are taken as literal
      character*8 stf,stavl
      character*1 c1,c2

      il1=len_trim(stf)
      il2=len_trim(stavl)

      iret=0; isp=1

      do
       c1=stf(isp:isp)
       c2=stavl(isp:isp)

       if(c2=="*".and.iallowwc==1)goto 10
       if(c1<c2)then
        iret=-1
        goto 20
       endif
       if(c1>c2)then
        iret=1
        goto 20
       endif

 10    continue
       isp=isp+1
       if(isp>il1.and.isp>il2)then
        iret=0
        goto 20
       endif
       if(isp>il2)then
        iret=1
        goto 20
       endif
       if(isp>il1)then
        iret=-1
        goto 20
       endif

      enddo

 20   istrcomp=iret

      return
      end function

!================================ STRIPEXT
      subroutine stripext(ffind)
      use strutilmod
      character*(*) ffind
      character*(len(ffind)) tmpstr

      tmpstr=ffind
      call ucase(tmpstr)
!      print *,"-",tmpstr,"-"
      ix=strfind(".",tmpstr,ib,ie,1,-1)
!      print *,ix
      if(ix==1)then
       do i=ib,len(tmpstr)
        tmpstr(i:i)=" "
       enddo
      endif
!      print *,"-",tmpstr,"-"
!      stop
      ffind=tmpstr
      return
      end subroutine
end module