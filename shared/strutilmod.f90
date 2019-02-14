module strutilmod
 integer,parameter:: parsecolslot=100, parsecollen=1400
 integer gparsecolcnt
 character gparsecol(parsecolslot)*(parsecollen)

 contains

!======================================= COLUMNPARSE
      subroutine columnparse(instring,sepchar,icomp,inoleft,iquote)
! ICOMP = 1 - remove zero-length columns (all spaces) from the results
!         0 - leave them in
!         2 - I want to divide columns by spaces. A group of spaces
!             together will count as one space.
!
! INOLEFT = 0 - remove leading spaces from columns
!           1 - leave leading spaces in 
!
! IQUOTE = 0 - I don't care where quotes are
!          1 - if I found a separation character, but this character
!              is in quotes, then don't separate here.
!
! 131105 - redid this subroutine to handle more than one column separator
!          it's also a bit faster since I don't use STRFIND
!
      use othermod
      character*(*) instring
      character sepchar*(*)
      integer ichop(1000)
      real*8 rsubtime

call subtimer("columnparse",1,rsubtime)
      gparsecolcnt=0

      ichopcnt=0

      idebug=isdebugactive("columnparse")

      ilen=len_trim(sepchar)
      ilens=len_trim(instring)
      if(ilens==0)goto 20

      gparsecolcnt=0
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvv"
       print *,"--- COLUMNPARSE ---"
       print "(' ',a,i0)","-"//trim(instring)//"-"//trim(sepchar)//"-   ilens: ",ilens
       print "(' ',a,i0)","inoleft:  ",inoleft
       print "(' ',a,i0)","icomp:  ",icomp
       print "(' ',a,i0)","inoleft:  ",inoleft
       print *,"   ---"
      endif

      if(icomp==2)then
       icol=0
       i=1
       iospc=1
       do
        if(instring(i:i)==" ")then
         ispc=1
        else
         ispc=0
        endif
        if(i>ilens)then
         if(iospc==1)goto 20
         ispc=1
         iospc=0
        endif
        if(ispc==0.and.iospc==1)then
         ibeg=i
        elseif(ispc==1.and.iospc==0)then
         icol=icol+1
         gparsecolcnt=icol
         gparsecol(icol)=instring(ibeg:i-1)
        endif
        if(i>ilens)goto 20
        i=i+1
        iospc=ispc
       enddo
       goto 20
      endif

      do ic=1,ilen
       ied=ilens
       do
        i=index(instring(1:ied),sepchar(ic:ic),.TRUE.)
        if(idebug==1)print "(' ',3a,i0)",":",sepchar(ic:ic),": ",i
        if(i==0)exit
        if(iquote==1)iqq=inquote(instring(1:ied),i)
        ied=i-1
        if(iquote==1.and.iqq==1)goto 30
        ichopcnt=ichopcnt+1
        ichop(ichopcnt)=i
30      continue
       enddo
      enddo

      do i1=1,ichopcnt-1
      do i2=i1+1,ichopcnt
       if(ichop(i1)>ichop(i2))then
        itmp=ichop(i1)
        ichop(i1)=ichop(i2)
        ichop(i2)=itmp
       endif
      enddo
      enddo

      if(idebug==1)then
       do i=1,ichopcnt
        print "(' ',i0)",ichop(i)
       enddo
      endif

      iop=1
      do i=1,ichopcnt+1
       if(i<=ichopcnt)then
        ip=ichop(i)-1
       else
        ip=ilens
       endif
       if(idebug==1)print "(2i5,a)",iop,ip,":"//instring(iop:ip)//":"
       if(icomp==1.and.ip<iop)goto 10
       gparsecolcnt=gparsecolcnt+1
       if(idebug==1)print "(' ',a,i0)","*****  gparsecolcnt:  ",gparsecolcnt
       gparsecol(gparsecolcnt)=instring(iop:ip)
 10    continue
       if(inoleft==0.and.ip>iop)then
        gparsecol(gparsecolcnt)=adjustl(gparsecol(gparsecolcnt))
       endif
! v v v v v v
! Since I don't want any string chopped off, check if the last character
! is a space, if not, warn that I need to increase PARSECOLLEN
       if(gparsecolcnt>0)then
        if(gparsecol(gparsecolcnt)(parsecollen:parsecollen)/=" ")then
         call enditalli("Increase PARSECOLLEN to at least: ",ip-iop)
        endif
       endif
! ^ ^ ^ ^ ^ ^
       iop=ichop(i)+1
      enddo


 20   if(idebug==1)then
       do i=1,gparsecolcnt
        print "(i3,' - ',a)",i,trim(gparsecol(i))
       enddo
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      endif
call subtimer("columnparse",2,rsubtime)

      return
      end subroutine


!==================  STRFIND  ==================
      function strfind(findit,instring,ibeg,iend,istart,istop)
! If IBEG = -99 then I am searching for spaces.
      use othermod
      character instring*(*),findit*(*)
      real*8 rsubtime
      
!vvvvvvv
!      istartxxx=istart
!      istopxxx=istop
!      ipoop1=strfind2(findit,instring,ibegxxx,iendxxx,istartxxx,istopxxx)
!^^^^^^^
      
call subtimer("strfind",1,rsubtime)
      idebug=isdebugactive("strfind")
      ifindlen=len_trim(findit)
      ilst=istart
      if(ilst.LT.1)ilst=1
      iret=0
      ispc=0
      if(ibeg==-99)ispc=1
      islen=len_trim(instring)
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv    strfind"
       print "(' ',3a,i0)","  findit:",trim(findit),": ",ifindlen
       if(ispc==1)print *,"!! space search !!"
       print *,"instring:",trim(instring),":"
       print *,"ist-chop:",trim(instring(ilst:)),":"
       print "(' ',a,i0)","startin: ",istart
       print "(' ',a,i0)"," start2: ",ilst
       print "(' ',a,i0)"," stop: ",istop
       ii=index(instring(istart:),trim(findit))
       print "(' ',a,i0)","index: ",ii
      endif
      ibeg=-1
      iend=-1

! If I'm searching for spaces, it has to be done
! "by hand" since spaces act weird in FORTRAN.
      if(ispc==1)then
       iloc=islen
       if(istop>-1)iloc=istop
       if(idebug==1)print "(' ',i0)",iloc
       isrow=0
       do i=istart,iloc
        if(instring(i:i)==" ")isrow=isrow+1
        if(instring(i:i)/=" ")isrow=0
        if(isrow==ifindlen)then
         iret=1
         iend=i
         ibeg=iend-ifindlen+1
         goto 22
        endif
       enddo
       goto 22
      endif

      i=index(instring(ilst:),trim(findit))
      iret=0
      if(i.gt.0)then
       i=i+ilst-1
       if(idebug==1)print "(' ',a,i0)","found at:  ",i
! if I found a match past the end of my string, exit
! this only happens if I'm finding spaces.
       if(i>islen)goto 22
       if(istop.lt.1.or.i.le.istop)then
        iret=1
        ibeg=i
        iend=ibeg+len_trim(findit)-1
       endif
      endif
 22   continue
      strfind=iret

      if(idebug==1)then
       print "(' ',a,3(' ',i0))","returning: ",iret,ibeg,iend
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    strfind"
      endif
call subtimer("strfind",2,rsubtime)
!      if(ibegxxx/=ibeg.or.iendxxx/=iend.or.istartxxx/=istart.or.istopxxx/=istop.or.ipoop1/=iret)then
!       iii=1
!      endif
      return
      end function

!============================================= BMATCH
      integer function bmatch(findit,instring,iwhere)
! IWHERE 1 - the start of the string
!        2 - the end of the string
      use othermod
      character*(*) instring
      character*(*) findit
      real*8 rsubtime
call subtimer("bmatch",1,rsubtime)
      idebug=isdebugactive("bmatch")
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv   bmatch"
       print "(3a)","-",trim(findit),"-"
       print "(3a)","-",trim(instring),"-"
      endif
      il=len_trim(findit)
      ils=len_trim(instring)
      iret=0
      if(il>ils)goto 10
      if(iwhere==1.and.instring(1:il)==findit)iret=1
      if(iwhere==2.and.instring(ils-il+1:ils)==findit)iret=1
!      bmatch=strfind(findit,instring,ibeg,iend,1,1)
 10   bmatch=iret
      if(idebug==1)then
       print *,iret
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   bmatch"
      endif
call subtimer("bmatch",2,rsubtime)
      return
      end function

!==================  REPLACECHAR  ==================
      subroutine replacechar(instring,findit,useitin)
      use othermod
      character*(*) instring
      character findit,useit,useitin*(*)
      real*8 rsubtime
call subtimer("replacechar",1,rsubtime)
      idebug=isdebugactive("replacechar")
      if(idebug==1)call debhed(1,"replacechar")
      ils=len_trim(instring)
      ilu=len(useitin)
      if(ilu==0)then
       useit=achar(0)
      else
       useit=useitin(1:1)
      endif
      if(idebug==1)then
       print "(' ',2a)",trim(instring),":"
       print *,"findit:",findit,":"
       print *,"useit:",useit,":"
       print "(' ',a,i0)","asc: ",ichar(useit)
       print "(' ',a,i0)","in len: ",ils
      endif
!     if useit is zero, then I want to delete this character from the string
      idelit=0
      if(ichar(useit).eq.0)idelit=1
      inl=len_trim(instring)
      do while(1.eq.1)
       i=index(instring,findit)
       if(idebug==1)print "(' ',i0,' ',i0)",i,inl
       if(i.eq.0)exit
       if(i>ils)exit
       if(i>inl)exit
       if(idelit.eq.0)then
        instring(i:i)=useit
       else
        instring=instring(1:i-1)//instring(i+1:)
        inl=len_trim(instring)
       endif
       if(idebug==1)print *,"[[[[ ",instring(1:len_trim(instring))
      enddo
      if(idebug==1)call debhed(0,"replacechar")
call subtimer("replacechar",2,rsubtime)
      return
      end subroutine

!==================  UCASE  ==================
      subroutine ucase(instring)
      use othermod
      character*(*) instring
      real*8 rsubtime
!      print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvv"
call subtimer("ucase",1,rsubtime)
      ilen=len_trim(instring)
      do i=1,ilen
       if(instring(i:i).ge.'a'.and.instring(i:i).le.'z')then
        ic=ichar(instring(i:i))
        ic=ic-32
        instring(i:i)=char(ic)
       endif
!       print *,instring(i:i),"-",ichar(instring(i:i))
      enddo
!      print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
call subtimer("ucase",2,rsubtime)
      return
      end subroutine

!==================  LCASEOLD  ==================
      subroutine lcaseold(instring)
      use othermod
      character*(*) instring
      real*8 rsubtime
!      print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvv"
call subtimer("lcaseold",1,rsubtime)
      ilen=len_trim(instring)
!      print *,ilen
      do i=1,ilen
       if(instring(i:i).ge.'A'.and.instring(i:i).le.'Z')then
        ic=ichar(instring(i:i))
        ic=ic+32
        instring(i:i)=char(ic)
       endif
!       print *,instring(i:i),"-",ichar(instring(i:i))
      enddo
!      print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
call subtimer("lcaseold",2,rsubtime)
      return
      end subroutine

!==================  NUM2STROLD  ==================
      function num2strold(rval,iw,idin,fillchar)
      use othermod
      character fillchar,fst*10,herest*20,retst*20,num2strold*20
      real*8 rsubtime
      
!vvvvvvv fixme
!      character strfart*20
!      call num2str3(rval,iw,idin,fillchar,strfart)
!^^^^^^^ fixme

call subtimer("num2strold",1,rsubtime)
      idebug=isdebugactive("num2strold")

      do i=1,20; num2strold(i:i)=" "; enddo

      ineg=0
      if(rval<0)then
       ineg=1
       rval=abs(rval)
      endif

      id=idin
      if(idin>9)id=9
      idec=1
      if(id==0)idec=0
      retst="                    "
      rrval=rval
      rmult=real(10**idin)
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvv"
       print *,"--- NUM2STROLD ---"
       print *,"rval: ",rrval,"  iw id: ",iw,".",id,"   fill:",fillchar,":     rmult: ",rmult,"    ineg:",ineg
      endif
      fst="(f0.9)"
      if(id==0)then
       fst="(i0)"
       write(herest,fst)int(rrval+.5)
       goto 10
      endif
      if(id==1)fst="(f0.1)"
      if(id==2)fst="(f0.2)"
      if(id==3)fst="(f0.3)"
      if(id==4)fst="(f0.4)"
      if(id==5)fst="(f0.5)"
      if(id==6)fst="(f0.6)"
      if(id==7)fst="(f0.7)"
      if(id==8)fst="(f0.8)"
      if(id==9)fst="(f0.9)"
!      write(herest,fst) int((rrval*rmult)+.51)/rmult
      write(herest,fst) int((rrval*rmult)+.50)/rmult
 10   continue
      if(ineg==1)herest="-"//trim(herest)
      ihslen=len_trim(herest)
      itargl=iw+id+idec
      if(itargl<ihslen)itargl=ihslen
      if(idebug==1)print *,"itargl: ",itargl,"   ihslen: ",ihslen,"    herest: ",herest
      do i=1,ihslen
       ii=(itargl-ihslen)+i
       if(idebug==1)print *,i,":",herest(i:i),":",ii,":"
       retst(ii:ii)=herest(i:i)
      enddo
      if(idebug==1)print *,"|",herest,"|",len_trim(herest),"|",itargl,"|",retst,"|"
! if my target is longer than my return, I need to pad with fillchar
      if(ihslen<itargl)then
       do i=1,itargl-ihslen
        retst(i:i)=fillchar
       enddo
      endif
      if(ineg==1)rval=-rval
      if(idebug==1)print *,"|",retst,"|"
      num2strold=retst
!vvvvvvv fixme
!      if(retst/=strfart)then
!       if(' '//strfart/=retst)then
!        print "(a)",trim(retst)
!        print "(a)",trim(strfart)
!        print "(f20.10)",rval
!        iii=1
!       endif
!      endif
!^^^^^^^ fixme
      if(idebug==1)print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
call subtimer("num2strold",2,rsubtime)
      return
      end function
!==================  ISNUM  ==================
      function isnum(c,idec,isign)
      use othermod
      character c
      real*8 rsubtime
call subtimer("isnum",1,rsubtime)
      ic=ichar(c)
      iret=0
      if(ic>=48.and.ic<=57)iret=1
! a decimal is a number if IDEC=1
      if(idec==1.and.ic==46)iret=1
! '-' and '+' are numbers if ISIGN=1
      if(ic==45.or.ic==43)then
       if(isign==1)iret=1
      endif
      isnum=iret
call subtimer("isnum",2,rsubtime)
      end function
!==================  ISLET  ==================
      function islet(c)
      use othermod
      character c
      real*8 rsubtime
call subtimer("islet",1,rsubtime)
      iret=0
      ic=ichar(c)
      if(ic>=65.and.ic<=90)iret=1
      if(ic>=97.and.ic<=122)iret=1
      islet=iret
call subtimer("islet",2,rsubtime)
      return
      end function

!=================== DELSTR ===================
      subroutine delstr(str,idelst,idellen)
      use othermod
      character str*(*)
      real*8 rsubtime
call subtimer("delstr",1,rsubtime)
      idebug=isdebugactive("delstr")
      if(idebug==1)then
       call debhed(1,"delstr")
       print "(' ',i0,' ',i0,' ',a)",idelst,idellen," >"//trim(str)//"<"
      endif
      str=str(:idelst-1)//str(idelst+idellen:)
      if(idebug==1)then
       print *," >"//trim(str)//"<"
       call debhed(0,"delstr")
      endif
call subtimer("delstr",2,rsubtime)

      return
      end subroutine

!=================== INSSTR ===================
      subroutine insstr(str,strins,loc,ispc)
      use othermod

! Inserts the string 'strins' into the string 'str' at position 'loc'.
! Characters in 'str' starting at position 'loc' are shifted right to
! make room for the inserted string. Trailing spaces of 'strins' are
! removed prior to insertion
! ISPC 0 - work like normal
!      1 - replace with spaces. The number of spaces is len_trim(strins)

      character(len=*):: str,strins
      character(len=len(str))::tempstr
      real*8 rsubtime
call subtimer("insstr",1,rsubtime)

      lenstrins=len_trim(strins)
      tempstr=str(loc:)
      call shiftstr(tempstr,lenstrins)
      tempstr(1:lenstrins)=strins(1:lenstrins)
      str(loc:)=tempstr
      if(ispc==1)then
       do i=1,lenstrins
        str(loc+i-1:loc+i-1)=" "
       enddo
      endif
call subtimer("insstr",2,rsubtime)
      return
      end subroutine

!=================== SHIFTSTR
      subroutine shiftstr(str,n)
      use othermod

! Shifts characters in in the string 'str' n positions (positive values
! denote a right shift and negative values denote a left shift). Characters
! that are shifted off the end are lost. Positions opened up by the shift
! are replaced by spaces.

      character(len=*):: str
      real*8 rsubtime

call subtimer("shiftstr",1,rsubtime)
      lenstr=len(str)
      nabs=iabs(n)
      if(nabs.ge.lenstr) then
        str=repeat(' ',lenstr)
        return
      end if
      if(n<0) str=str(nabs+1:)//repeat(' ',nabs)  ! shift left
      if(n>0) str=repeat(' ',nabs)//str(:lenstr-nabs)  ! shift right
call subtimer("shiftstr",2,rsubtime)
      return
      end subroutine shiftstr

!====================== SUBSTR
      subroutine substr(str,find,repl,iwhich)
      use othermod
      character*(*) str,find,repl
      real*8 rsubtime

call subtimer("substr",1,rsubtime)
      idebug=isdebugactive("substr")

      irepllen=len_trim(repl)
      ifindlen=len_trim(find)
      if(idebug==1)then
       print *,"vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  substr"
       print *,"str :",trim(str),":"
       print *,"find:",find,":    len:",ifindlen
       print *,"repl:",repl,":   len:",irepllen
      endif
      imatch=0
      ist=1
      do while(1==1)
       ix=strfind(find,str,ib,ie,ist,-1)
       imatch=imatch+1
       if(idebug==1)print *,"ix,ib,ie,iwhich,imatch,ist: ",ix,ib,ie,iwhich,imatch,ist
       if(ix==0)exit
       isubit=0
       if(imatch==iwhich)isubit=1
       if(iwhich==0)isubit=1
       if(isubit==1)then
        call delstr(str,ib,ifindlen)
        if(idebug==1)then
         print *,"                       111111111122222222223333333333"
         print *,"              123456789012345678901234567890123456789"
         print *,"after delstr :",trim(str),":"
        endif
        call insstr(str,repl,ib,0)
        if(idebug==1)print *,"after insstr :",trim(str),":"
        ist=ist+irepllen
        if(imatch==iwhich)goto 10
       else
        ist=ib+1
       endif
       if(idebug==1)print *,"next ist: ",ist
      enddo
 10   continue
      if(idebug==1)then
       print *,"^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  substr"
      endif
call subtimer("substr",2,rsubtime)
      return
      end subroutine
!========================= TRIMSPC
      subroutine trimspc(str)
      use othermod
      character*(*) str
      real*8 rsubtime
      il=len_trim(str)
call subtimer("trimspc",1,rsubtime)
!      print *,il
      do i=1,il
       if(str(i:i)/=" ")exit
      enddo
!      print *,"i: ",i
!      print *,":",trim(str),":"
      str=str(i:)
!      print *,":",trim(str),":"
!      do ii=1,il-i+1
!       str(ii:ii)=str(ii+i-1:ii+i-1)
!      enddo
!      do iii=ii,il
!       str(iii:iii)=" "
!      enddo
call subtimer("trimspc",1,rsubtime)
      return
      end subroutine

!C=========================== INT2HEX
!      subroutine int2hex(ival,hex,ilen)
!      character loc*8
!      character*(*) hex
!
!      write(loc,"(z8)"),ival
!      call delstr(loc,1,8-ilen)
!      do i=1,8
!       if(loc(i:i)==" ")then
!        loc(i:i)="0"
!       else
!        exit
!       endif
!      enddo
!      hex=loc
!      return
!      end
!==================  NUM2STRIOLD  ==================
      function num2striold(ival)
      use othermod
      character herest*20,num2striold*20
      real*8 rsubtime
      
call subtimer("num2striold",1,rsubtime)
      do i=1,20; num2striold(i:i)=" "; enddo

      ineg=0
      if(ival<0)then
       ineg=1
       ival=abs(ival)
      endif

      write(herest,"(i0)")ival
      if(ineg==1)call insstr(herest,"-",1,0)
      if(ineg==1)ival=-ival
      num2striold=herest
call subtimer("num2striold",2,rsubtime)
      return
      end function
!========================================== FITIT2
      subroutine fitit2(str1,il1,ispc,str2,il2,str)
! Build string STR out of two strings, keeping each string
! in its own column. IL1 is the width of string STR1, followed
! by ISPC spaces of padding, then STR2 in a width of IL2. If
! either string is longer than its column width, the end of the
! long string is replaced with "..."
! The output of repeated calls could look like this if
! column one is 11 wide, two spaces, and column two is 10 wide.
!
!  Bob Smith    Kicker
!  Luke Jon...  Punter
!  Steve Green  Linebac...

      use othermod
      character str*(*),str1*(*),str2*(*),loc*1000
      real*8 rsubtime

call subtimer("fitit2",1,rsubtime)
      str=" "
      ial1=len_trim(str1)
      loc=str1
      if(ial1>il1)loc=str1(1:il1-3)//"..."

      ix=0
      do i=1,il1
       ix=ix+1
       if(i>ial1)then
        str(ix:ix)=" "
       else
        str(ix:ix)=loc(i:i)
       endif
      enddo
      do i=1,ispc
       ix=ix+1
       str(ix:ix)=" "
      enddo
      ial2=len_trim(str2)
      loc=str2
      if(ial2>il2)loc=str2(1:il2-3)//"..."
      do i=1,il2
       ix=ix+1
       if(i>ial2)then
        str(ix:ix)=" "
       else
        str(ix:ix)=loc(i:i)
       endif
      enddo
call subtimer("fitit2",2,rsubtime)

      return
      end subroutine

!========================================== FITIT
      subroutine fitit(strin,strout,il)
! This routind will fit a long string into a short string by printing
! the first and last parts of the string with ... in the middle.
! For instance, if my string is "Hello there!" and my target width is 8,
! then I will get "Hel...e!" If my original string is less than target
! width, I just get the original string.
      use othermod
      character strin*(*),ell*4,strout*(*)
      real*8 rsubtime
call subtimer("fitit",1,rsubtime)

      idebug=isdebugactive("fitit")
      if(idebug==1)then
       call debhed(1,"fitit")
       print *,"strin:"//trim(strin)//":"
       print *,"il: ",il
       print *,len(strout)
      endif

      if(len(strout)<il)call enditall("FITIT: STROUT not long enough to hold IL characters.")

      ilin=len_trim(strin)
      if(idebug==1)print *,"ilin: ",ilin
      if(ilin<=il)then; strout=strin; goto 10; endif

      strout=" "
      ell="___"; ie=3
      if(mod(il-3,2)==1)then; ell="____"; ie=4; endif
      ichop=(il-ie)/2
      if(idebug==1)print *,"ichop: ",ichop,"  :"//strin(ilin-ichop+1:ilin)//":"
      strout=strin(1:ichop)//trim(ell)//strin(ilin-ichop+1:ilin)

 10   continue

      if(idebug==1)then
       print *,"strout:"//trim(strout)//":"
       call debhed(0,"fitit")
      endif
call subtimer("fitit",2,rsubtime)
      return
      end subroutine

!====================================== ITEXTSAME
      function itextsame(str1,str2,rperc)
! This uses the Levenshtein method for determining how similar strings are.
! The lower the value returned, the more similar two strings are. The
! Levenshtein value is the total number of insertions, deletions, or
! substitutions needed to "transform" on string into another
! RPERC - output, the percentages the strings are alike
      use othermod
      character*(*) str1,str2
! Increasing ISLEN will let you use bigger strings, but it will
! also increase the memory needed to do so exponentially. Be
! VERY careful with increasing this value.
      integer,parameter::islen=5000
      integer id(0:islen,0:islen)
      real*8 rsubtime
call subtimer("itextsame",1,rsubtime)

      if(len_trim(str1)>islen.or.len_trim(str1)>islen)then
       it1=len_trim(str1)
       it2=len_trim(str2)
       ix=it1
       if(it2>ix)ix=it2
       call enditall("ITEXTSAME: Increase ISLEN "//trim(num2stri(ix)))
      endif


      il1=len_trim(str1)
      il2=len_trim(str2)
      imaxl=il1
      if(il2>imaxl)imaxl=il2

      do i=0,il1
       id(i,0)=i
      enddo
      do j=0,il2
       id(0,j)=j
      enddo
      iii=1

      do j=1,il2
       do i=1,il1
        if(str1(i:i)==str2(j:j))then
         id(i,j)=id(i-1,j-1)
        else
         idel=id(i-1,j)+1
         iins=id(i,j-1)+1
         isub=id(i-1,j-1)+1
         iv=idel
         if(iins<iv)iv=iins
         if(isub<iv)iv=isub
         id(i,j)=iv
        endif
       enddo
       iii=1
      enddo
      
35    continue
!      do i=0,il1
!       do j=0,il2
!        print "(i3$)",id(i,j)
!       enddo
!       print "(a)",""
!      enddo
      
      rperc=(1-(id(il1,il2)/(imaxl*1.0)))
      itextsame=id(il1,il2)
call subtimer("itextsame",2,rsubtime)

      return
      end function

!=================================== INITSTR
      subroutine initstr(str)
      use othermod
      character str*(*)
      real*8 rsubtime
      call subtimer("initstr",1,rsubtime)

      il=len(str)
      do i=1,il; str(i:i)=" "; enddo
!      print *,"initstr size: ",il
      return
      call subtimer("initstr",2,rsubtime)
      end subroutine

!=============================== STR2NUMOLD
      subroutine str2numold(str,ival,rval,isrch)
      use othermod
      character str*(*),str2*500,fmt*10,fmtp*10
      character dfa(20)*2,ffa(20)*2
      real*8 rsubtime
      data dfa/"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"/
      data ffa/"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"/
      
      call subtimer("str2numold",1,rsubtime)
      str2=str
! ISRCH will give me a ROUGH idea of where the number starts.
! I need to make sure I'm starting on a number
      if(isrch>0)then
       il=len_trim(str)
       do i=isrch,il
        if(str(i:i)=="+")goto 10
        if(str(i:i)=="-")goto 10
        if(str(i:i)==".")goto 10
        ic=ichar(str(i:i))
        if(ic>=48.and.ic<=57)goto 10
       enddo
! If I'm here, I didn't find a valid number from the search point.
       call enditalli("No valid number found! >"//trim(str)//"<",isrch)
10     continue
! At this point, I have a number OR a +/-/. sign. Increment the pointer
! and continue searching.
       i1=i+1
       idecf=0
       iprec=0
       do i=i1,il
! +/- signs no longer count as a valid character. If they're present, they should
! be at the START of the number in the previous column. If I find them past that
! point, I'm assuming they're column separaters or something.
!        if(str(i:i)=="+")cycle
!        if(str(i:i)=="-")cycle
        if(str(i:i)==".")then; idecf=1; cycle; endif
        ic=ichar(str(i:i))
        if(ic>=48.and.ic<=57)then
         iprec=iprec+idecf
         cycle
        endif
        exit
       enddo
       str2=str(i1-1:i-1)
      else
       il=len_trim(str)
       ix=index(str,".")
       idecf=0
       iprec=0
       if(ix>0)then
        idecf=1
        iprec=il-ix
       endif
      endif
      i4=len_trim(str2)
      if(idecf==1)then
       if(i4>0.and.i4<=20)then
        fmt=dfa(i4)
       else
        write(fmt,"(i10)"),i4
       endif
       if(iprec>0.and.iprec<=20)then
        fmtp=dfa(iprec)
       else
        write(fmtp,"(i10)"),iprec
       endif
       fmt="(f"//trim(adjustl(fmt))//"."//trim(adjustl(fmtp))//")"
       read(str2,fmt,iostat=istat),rval
       isign=1
       if(rval<0)isign=-1
       rval=abs(rval)
       ival=rval+.5
       ival=ival*isign
       rval=rval*real(isign)
      else
! I'm using arrays instead of WRITE because they're faster
       if(i4>0.and.i4<=20)then
        fmt="(i"//dfa(i4)//")"
       else
        write(fmt,"(i10)"),i4
        fmt="(i"//trim(adjustl(fmt))//")"
       endif
       read(str2,fmt,iostat=istat),ival
       rval=ival
      endif
      call subtimer("str2numold",2,rsubtime)
      return
      end subroutine
      
!============================= STRINIT
      subroutine strinit(str)
      use othermod
      character str*(*)
      real*8 rsubtime
      call subtimer("strinit",1,rsubtime)
      il=len(str)
      do i=1,il
       str(i:i)=" "
      enddo
      call subtimer("strinit",2,rsubtime)
      return
      end subroutine
      
!======================== STRCMP
      subroutine strcmp(str1,str2,i1,i2,i3,i4)
! This routine compares str1 to str2 and tells me how it matches
! I1 - exact match
! I2 - exact match if case is ignored
! I3 - str1 is found in str2
! I4 - str1 is found in str2, if case is ignored
      use othermod
      character str1*(*),str2*(*)
      character str1l*(len(str1))
      character str2l*(len(str2))
      real*8 rsubtime
      call subtimer("strcmp",1,rsubtime)
      i1=0; i2=0; i3=0; i4=0
      if(str1==str2)then
       i1=1; i2=1; i3=1; i4=1
       goto 99
      endif
      if(strfind2(str1,str2,ib,ie,1,-1)==1)then
       i3=1; i4=1
       goto 99
      endif
      str1l=str1; str2l=str2
      call lcase(str1l)
      call lcase(str2l)
      if(str1l==str2l)then
       i2=1; i4=1
       goto 99
      endif
      if(strfind2(str1l,str2l,ib,ie,1,-1)==1)then
       i4=1
       goto 99
      endif
99    continue      
      call subtimer("strcmp",2,rsubtime)
      return
      end subroutine
      
!============================= NUM2STR
      function num2str(rin,iwin,id,fc)
! This is a rewrite of NUM2STR that's easier to follow and is ~10% faster
      use othermod
      character fc,dfa(20)*2,ffa(20)*2,wf*10,num2str*20,str*20
      real*8 rsubtime
      data dfa/"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"/
      data ffa/"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"/
      
call subtimer("num2str",1,rsubtime)

      iwpart=0; idpart=0; rdpart=0; wf=""; ineg=0

! If I don't have a width, make it as long as the string
! minus decimal places and sign plus one extra just for fun.
      iw=iwin
      if(iw==0)then
       ils=len(str)
       iw=ils-(id+3)
      endif
      ineg=1
      rv=rin
      if(rv<0)ineg=-1
      rv=abs(rv)
      
! Get the integer part
      iwpart=ifix(rv)
! Make the decimal part an integer first. This will help with rounding.
      idpart=(((rv-float(iwpart))*float((10**id)))+.5)
! ... and then convert it back to a decimal.
      rdpart=float(idpart)/(10**id)
! If I want a float value with a decimal place...
      if(id>0)then
       itl=id+iw+2
       if(itl>20.or.id>20)call enditall("NUM2STR3: String too big!")
       wf='(f'//trim(adjustl(ffa(itl)))//'.'//trim(adjustl(dfa(id)))//')'
       write(str,wf),(float(iwpart)+rdpart)*ineg
      else
! If I just want an integer with NO decimal point.
       itl=iw+1
       if(itl>20)call enditall("NUM2STR3: String too big!")
       wf='(i'//trim(adjustl(ffa(itl)))//')'
       write(str,wf),iwpart*ineg
      endif
! If I'm padding with a leading character, do that here.
      if(fc==" ")goto 10
      do i=2,itl
       if(str(i:i)==" ")then
        str(i:i)=fc
       else
        exit
       endif
      enddo
10    continue
      str=adjustl(str)
! If this number is positive, AND my original IW was 0, AND the first character is 0,
! then I bet this is a win % of some sort in which case I do NOT want a leading zero.
      if(rin>=0.0)then
       if(iwin==0)then
        if(str(1:1)=='0')then
         str=str(2:)
        endif
       endif
      endif
      num2str=str
call subtimer("num2str",2,rsubtime)
      
      return
      end function
      
!================================== NUM2STRI
      function num2stri(iv)
! This is a rewrite of NUM2STRI that's easier to follow and is ~10% faster
      use othermod
      character num2stri*20
      real*8 rsubtime
call subtimer("num2stri",1,rsubtime)
      write(num2stri,"(i10)"),iv
      num2stri=adjustl(num2stri)
call subtimer("num2stri",2,rsubtime)
      return
      end function
      
!=============================== STR2NUM
      subroutine str2num(str,ival,rval,isrch)
!  This is a rewrite of STR2NUM that's easier to follow and ~20% faster.
      use othermod
      character str*(*),fmt*10,ffa(20)*2,fmti*10
      real*8 rsubtime
      data ffa/"1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"/
      call subtimer("str2num",1,rsubtime)
      rval=0.0; ival=0
      ils=len_trim(str)
      if(ils==0)goto 11
!   If I'm searching, look for the start of the number
      if(isrch>0)then
       do i=isrch,ils
        if(str(i:i)=="+")goto 10
        if(str(i:i)=="-")goto 10
        if(str(i:i)==".")goto 10
        ic=ichar(str(i:i))
        if(ic>=48.and.ic<=57)goto 10
       enddo
! If I'm here, I didn't find a valid number from the search point.
!       call enditalli("No valid number found! >"//trim(str)//"<",isrch)
! Instead of stopping like I used to, just return zeroes.
       goto 11
10     continue
!   Look for the end of the number. '+' and '-' are no longer valid
!   because they should be at the start of the number. If I find them
!   after, they might be column delimiters.
       do i2=i+1,ils
        if(str(i2:i2)==".")cycle
        ic=ichar(str(i2:i2))
        if(ic>=48.and.ic<=57)cycle
        exit
       enddo
       i2=i2-1
      else
!   If I'm not searching, the bounds are the entire string.
       i=1
       i2=ils
      endif
      il=(i2-i)+1
      if(il>=1.and.il<=20)then
       fmt='(f'//ffa(il)//'.0)'
       fmti='(i'//ffa(il)//')'
      else
       write(fmt,"('(f',i2,'.0)')"),il
       write(fmti,"('(i',i2,')')"),il
      endif
      read(str(i:i2),fmt,iostat=istatf),rval
      read(str(i:i2),fmti,iostat=istati),ival
!   If ISTATI is 0, good read!
      if(istati==0)goto 11
!   If float read is 64 or 62, I tried to read a string.
      if(istatf==64.or.istatf==62.or.istatf==5010.or.istati==5010)then
       rval=0.0
       ival=0
       goto 11
      endif
!   If float read is good, but int is bad, then this is a 
!   decimal number and can't be read as integer.
      if(istatf==0.and.istati>0)then
       if(rval>0.0)then
        ival=ifix(rval+0.5)
       else
        ival=ifix(rval-0.5)
       endif
       goto 11
      endif
      print "(a)","STR: >"//trim(str)//"<"
      print "('Start/End: ',2i10)",i,i2
      call enditallii("Bad STR2NUM read: ",istati,istatf)
11    continue
!      if(rval>0)then
!       ival=ifix(rval+0.5)
!      else
!       ival=ifix(rval-0.5)
!      endif
      call subtimer("str2num",2,rsubtime)
      return
      end subroutine
      
!=================================== STRFIND2
      function strfind2(findit,instring,ibeg,iend,istart,istop)
      use othermod
      character*(*) findit,instring
      integer strfind2
      real*8 rsubtime
            
      call subtimer("strfind2",1,rsubtime)
      if(ibeg==-99)call enditall("Use STRFIND for space searching until I can get STRFIND2 working.")
      
      ibeg=-1; iend=-1
      
      i1=istart
      if(istart<=0)i1=1
      i2=istop
      if(istop<=0)i2=len(instring)
      if(i1>i2)call swapi(i1,i2)
      
      ix=index(instring(i1:i2),trim(findit))
      iret=0
      if(ix<=0)goto 99
      
      iret=1
      ibeg=ix+i1-1
      iend=ibeg+len(findit)-1
      
99    continue
      strfind2=iret
      call subtimer("strfind2",2,rsubtime)
      
      return
      end function
      
!==================  LCASE  ==================
      subroutine lcase(instring)
      use othermod
      character*(*) instring
      real*8 rsubtime
call subtimer("lcase",1,rsubtime)
      ilen=len_trim(instring)
      do i=1,ilen
       ic=ichar(instring(i:i))
       if(ic>90)cycle
       if(ic<65)cycle
       instring(i:i)=achar(ic+32)
      enddo
call subtimer("lcase",2,rsubtime)
      return
      end subroutine
      
! 04/25/89 BAS  CREATION DATE
! 03/24/97 SMG  ADD UTILITY FOR MATCHING STING TO WILD CARD STRING
! 11/10/98 BAS  B981110B MODIFY IF STATEMENT TO PREVENT RANGE ERROR ON SGI
! 11/10/98 BAS  B981110B MODIFY IF STATEMENT TO PREVENT RANGE ERROR ON SGI
! 08/02/00 CSA  M000801A IFUNC 1 NO LONGER IGNORES TRAILING CONTROL CHARACTERS
! 01/24/01 KAA  B020122A AVOID OUT OF BOUND ERROR WHEN MATCHING W/WILD CARD
! 05/28/08 GSS E080520Z SPECIAL DXF TEXT PROCESSING
! 5/11/10  GSS B100510Z COMPILER ISSUES
! 2/21/17  APM M170221Z ADDED UTILITIES 13-16 FOR DAMCO.
!
!************************** STRUTL  ****************************
      SUBROUTINE STRUTL(IFUNC,MSTR1,MSTR2,IP,RP,NP,IOUT)
!***************************************************************
!  PURPOSE: TO PROVIDE UTILITIES FOR STRING MANIPULATION
!
!  INPUT: IFUNC
!         MSTR1,MSTR2,IP,RP,NP: THESE ARGUMENTS ARE DEPENDENT ON IFUNC
!  OUTPUT:
!         MSTR1,MSTR2,IP,RP,NP: THESE ARGUMENTS ARE DEPENDENT ON IFUNC
!         IOUT:  VALUE DEPENDS ON IFUNC
!
!  METHOD:
!    UTILITY  1: FIND LENGTH OF A STRING WITH NO TRAILING SPACES.
!    UTILITY  2: CHECK IF A STRING CONTAINS NUMBERS ONLY.
!    UTILITY  3: CHECK IF A STRING CONTAINS ALPHABETS ONLY.
!    UTILITY  4: REMOVE SPACES FROM A STRING AND REPORT ITS LENGTH.
!    UTILITY  5: CHECK IF STRING 1 IS MADE OF CHARACTERS IN STRING 2.
!    UTILITY  6: CONVERT A STRING TO A NUMBER.
!    UTILITY  7: CONVERT ANY LOWER CASE LETTERS TO UPPER CASE
!    UTILITY  8: CONVERT ANY UPPER CASE LETTERS TO LOWER CASE
!    UTILITY  9: REMOVE LEADING SPACES FROM STRING MSTR1
!    UTILITY 10: RIGHT JUSTIFY TEXT IN STRING MSTR1
!    UTILITY 11: CHECK IF A STRING CONTAINS NUMBERS ONLY ('+', '-', '.' ARE OK)
!    UTILITY 12: CHECK IF STRING 1 MATCHES WILD CARD STRING 2
!                NOTE: STRING 1 SHOULD NOT CONTAIN THE WILD CARD CHARACTERS
!                      '*' OR '?'
!    UTILITY 13: FILL A STRING WITH ALL SPACES (INITIALIZE IT)
!    UTILITY 14: REPLACE ANY CHARACTER IN MSTR1 LESS THAN ASCII 32
!                IF IP=0, THEN JUST REPLACE WITH A SPACE
!                IF IP=1, THEN SCOOT THE STRING OVER TO FILL THE VOID
!    UTILITY 15: MSTR1 IS A VERY LONG STRING, BUT I NEED TO CRAM IT
!                INTO A VERY SMALL SPACE, FOR EXAMPLE, TO PRINT IT. I
!                NEED TO DISPLAY AS MUCH OF THE STRING AS POSSIBLE,
!                BUT I NEED TO HINT THAT THERE IS MORE BY USING AN
!                ELLIPSES.
!                IP - THE MAXIMUM LENGTH MSTR1 CAN BE
!                RP - 1.0 PUT THE ELLIPSES AT THE BEGINNING
!                     2.0 PUT THE ELLIPSES AT THE END
!    UTILITY 16: REPLACE ANY NON-FRIENDLY UNIX CHARACTERS WITH UNDERSCORES
!
!  KEYS: STRING UTILITIES
!
!  EXTERNAL REFERENCES: CONVRT
!
!  CALLED FROM:  UTILITY
!
!  IMPLICITS:
!      INCLUDE 'AQIMPL'
!
!  DIMENSIONS:
      DIMENSION KNPUT(132)
!
!  INTEGER DECLARATIONS:
!
!  CHARACTER DECLARATIONS:
      CHARACTER*(*) MSTR1,MSTR2
      CHARACTER*26  MUPPER,MLOWER
      CHARACTER*132  MTEMP
      character np
      
      integer K
!
!  LOGICAL DECLARATIONS:
!
!  REAL DECLARATIONS:
!
!  COMMON STATEMENTS:
!      INCLUDE 'AQCOMABC'
!
!  DATA STATEMENTS:
      DATA MLOWER/'abcdefghijklmnopqrstuvwxyz'/
      DATA MUPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
!
!*************** START EXECUTABLE CODE ***************************
!
      IOUT=0
!
      ILEN=LEN(MSTR1)
!
      IF ( IFUNC.LT.1 .OR. IFUNC.GT.16 ) GOTO 999
!
      GOTO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160)IFUNC
!
!***** IFUNC=1: IOUT IS THE LENGTH OF STRING WITH NO TRAILING BLANKS
!
 10   IOUT=ILEN+1
 11   IOUT=IOUT-1
      IF(IOUT.GT.0)THEN
        IF(ICHAR(MSTR1(IOUT:IOUT)).EQ.KCSP)GOTO 11
      ENDIF
      GOTO 999
!
!***** IFUNC=2: IOUT=1 WHEN STRING CONTAINS NUMBERS ONLY
!
 20   I=0
 23   I=I+1
      IF(I.GT.ILEN)GOTO 999
      K=ICHAR(MSTR1(I:I))
      IF(K.GE.KC0.AND.K.LE.KC9)THEN
        IOUT=1
      ELSE
        IOUT=0
        GOTO 999
      ENDIF
      GOTO 23
!
!***** IFUNC=3: IOUT=1 WHEN STRING CONTAINS ALPHABETS ONLY
!
 30   I=0
 33   I=I+1
      IF(I.GT.ILEN)GOTO 999
      K=ICHAR(MSTR1(I:I))
      IF((K.GE.KCA.AND.K.LE.KCZ).OR.(K.GE.KCSA.AND.K.LE.KCSZ))THEN
        IOUT=1
      ELSE
        IOUT=0
        GOTO 999
      ENDIF
      GOTO 33
!
!***** IFUNC=4: IOUT IS THE LENGTH OF STRING WITHOUT SPACES
!
 40   I=0
      IOUT=0
 43   I=I+1
      IF(I.GT.ILEN) THEN
        IF ( IOUT.LT.ILEN ) THEN
          MSTR1(IOUT+1:ILEN)=' '
        END IF
        GOTO 999
      ENDIF
      IF(ICHAR(MSTR1(I:I)).EQ.KCSP)GOTO 43
      IOUT=IOUT+1
      MSTR1(IOUT:IOUT)=MSTR1(I:I)
      GOTO 43
!
!***** IFUNC=5: IOUT=1 IF CHARACTERS OF MSTR2 MAKE UP MSTR1
!
 50   ILEN2=LEN(MSTR2)
      I=0
 53   I=I+1
      IF(I.GT.ILEN)GOTO 999
      DO 55 ICTR2=1,ILEN2
        IF(ICHAR(MSTR1(I:I)).EQ.ICHAR(MSTR2(ICTR2:ICTR2)))THEN
          IOUT=1
          GOTO 53
        ENDIF
 55   CONTINUE
      IOUT=0
!
!***** IFUNC=6: CONVERT CHARACTER STRING MSTR1 TO A NUMBER
! (RP = REAL VALUE/IP = INTEGER).  IOUT=1 IF MSTR1 CAN BE CONVERTED
! TO A NUMBER
!
 60   DO 63 I=1,ILEN
        KNPUT(I)=ICHAR(MSTR1(I:I))
 63   CONTINUE
!      CALL CONVRT(KNPUT,1,ILEN,INMBER,REEL,IXX,IERR)
      IF(IERR.EQ.0)IOUT=1
      RP=REEL
      IP=INMBER
      GOTO 999
!
!***** IFUNC=7: CONVERT ANY LOWER CASE LETTERS TO UPPER CASE: a => A
!
  70  CONTINUE
      DO 77 I=1,ILEN
         IPOS=INDEX(MLOWER,MSTR1(I:I))
         IF(IPOS.GT.0) MSTR1(I:I)=MUPPER(IPOS:IPOS)
  77  CONTINUE
      GOTO 999
!
!***** IFUNC=8: CONVERT ANY UPPER CASE LETTERS TO LOWER CASE: A => a
!
  80  CONTINUE
      DO 88 I=1,ILEN
         IPOS=INDEX(MUPPER,MSTR1(I:I))
         IF(IPOS.GT.0) MSTR1(I:I)=MLOWER(IPOS:IPOS)
  88  CONTINUE
      GOTO 999
!
!***** IFUNC=9: REMOVE LEADING SPACES FROM STRING MSTR1
!
  90  IOUT=0
      DO 93 I=1,ILEN
         IF(ICHAR(MSTR1(I:I)).NE.KCSP)THEN
            DO 98 J=I,ILEN
               IOUT=IOUT+1
               MSTR1(IOUT:IOUT)=MSTR1(J:J)
  98        CONTINUE
!
!           BLANK OUT REMAINING STRING
            DO 99 JJ=(IOUT+1),ILEN
               MSTR1(JJ:JJ)=' '
  99        CONTINUE
            GOTO 999
         ENDIF
  93  CONTINUE
      GOTO 999
!
!***** IFUNC=10: RIGHT JUSTIFY TEXT IN STRING MSTR1
!
 100  IOUT=0
      ITLEN=ILEN+1
 105  ITLEN=ITLEN-1
      IF(ITLEN.GT.0)THEN
        IF(ICHAR(MSTR1(ITLEN:ITLEN)).LE.KCSP) GOTO 105
      ENDIF
      IF ( ITLEN.GT.0 .AND. ITLEN.LT.ILEN ) THEN
        MTEMP=MSTR1
        MSTR1=' '
        MSTR1((ILEN-ITLEN)+1:ILEN)=MTEMP(1:ITLEN)
      ENDIF
      GOTO 999
!
!***** IFUNC=11: IOUT=1 WHEN STRING CONTAINS NUMBERS OR '+', '-', '.' ONLY
!
 110  I=0
 115  I=I+1
      IF(I.GT.ILEN) THEN
         IF(IOUT.EQ.2) IOUT=1
         GOTO 999
      ENDIF
      K=ICHAR(MSTR1(I:I))
      IF((K.GE.48.AND.K.LE.57) .OR.K.EQ.43 .OR. K.EQ.45 .OR. K.EQ.46) THEN
        IF(IOUT.EQ.2) THEN
!  SPACE FOUND IN MIDDLE OF STRING - ALLOW BEGINNING AND TRAILING SPACES
           IOUT=0
           GO TO 999
        ENDIF
        IOUT=1
      ELSEIF(K.EQ.32) THEN
        IF(IOUT.EQ.1)IOUT=2
      ELSE
        IOUT=0
        GOTO 999
      ENDIF
      GOTO 115
!
!**** IFUNC= 12: IOUT=1 IF STRING 1 MATCHES WILD CARD STRING 2
!                THE WILD CARD STRING MAY CONTAIN THE FOLLOWING WILD CARD
!                CHARACTERS:
!                    '*' - MATCHES ZERO OR MORE CHARACTERS
!                    '?' - MATCHES EXACTLY ONE CHARACTER
!
 120  ILEN2=LEN(MSTR2)
      IP=1
      IN=1
      IDONE=0
      IPSAVE=-1
      INSAVE=0
      IMATCH=0
!
! FIND FIRST NON-SPACE CHARACTER FROM RIGHT IN THE WILD CARD STRING
!
      I=ILEN2
 122  IF(I.GT.0) THEN
         IF(MSTR2(I:I).EQ.' ') THEN
            I=I-1
            GOTO 122
         ENDIF
      ENDIF
      IF(I.EQ.0) GOTO 999
!
 125  IF(IDONE.EQ.0) THEN
!
!     CHECK FOR THE '?' WILD CARD CHARACTER
!
         IF(MSTR2(IP:IP).EQ.'?') THEN
            IF(IN.EQ.ILEN) THEN
               IMATCH=0
               IDONE=1
            ELSE
               IP=IP+1
               IN=IN+1
            ENDIF
!
!     CHECK FOR THE '*' WILD CARD CHARACTER
!
         ELSEIF(MSTR2(IP:IP).EQ.'*') THEN
            IF(MSTR2(IP+1:IP+1).NE.'*') THEN
               IPSAVE=IP
               INSAVE=IN+1
            ENDIF
            IP=IP+1
!
!     CHECK FOR IDENTICAL CHARACTERS
!
         ELSEIF(MSTR2(IP:IP).EQ.MSTR1(IN:IN)) THEN
            IF(IN.EQ.ILEN) THEN
               IMATCH=1
               IDONE=1
            ELSEIF(IP.EQ.ILEN2.AND.(MSTR2(I:I).EQ.'*'.OR.MSTR2(I:I).EQ.'?')) THEN
!
! THE STRING MATCHES THE PATTERN
               IMATCH=1
               IDONE=1
            ELSE
               IP=IP+1
               IN=IN+1
            ENDIF
!
!     CHECK FOR A WILD CARD PATTERN STRING THAT'S TOO LONG
!
         ELSEIF(IP.LT.ILEN2 .AND.IN.EQ.ILEN) THEN
            IMATCH=0
            IDONE=1
!
!     CHARACTERS DON'T MATCH. BACK UP TO THE LAST '*' WILD CARD
!
         ELSE
            IP=IPSAVE
            IN=INSAVE
            IF(IP.EQ.-1.OR.IP.GT.ILEN2.OR.IN.GT.ILEN) THEN
               IMATCH=0
               IDONE=1
            ENDIF
         ENDIF
         GOTO 125
      ENDIF
      IOUT=IMATCH
      GOTO 999
      
130   CONTINUE
! FILL STRING WITH SPACES
      IL=LEN(MSTR1)
      DO I=1,IL
         MSTR1(I:I)=' '
      ENDDO
      GOTO 999
      
140   CONTINUE
! REPLACE CONTROL CHARACTERS WITH SPACES
      IL=LEN(MSTR1)
      DO I=1,IL
         IC=ICHAR(MSTR1(I:I))
         IF(IC.LT.32)THEN
            MSTR1(I:I)=' '
!   SCOOT THE STRING OVER TO FILL THE SPACE
            IF(IP.EQ.1.AND.I.LT.IL)THEN
             MSTR1(I:)=MSTR1(I+1:)
!   PUT A SPACE AT THE END JUST SO NO JUNK GETS PULLED IN
             MSTR1(IL:IL)=' '
            ENDIF
         ENDIF
      ENDDO
      GOTO 999
!
150   CONTINUE
! PRINT AS MUCH OF MSTR1 AS POSSIBLE IN THE SPACE SIZE
! DEFINED BY IP.
      IL=LEN_TRIM(MSTR1)
! IF YOU'VE GIVEN ME A STRING LENGTH SHORTER THAN AN ELLIPSES,
! THAT'S DUMB, BUT I NEED TO HANDLE IT. I ALSO NEED TO HANDLE
! A SIZE OF THREE AS THAT WILL TRY TO ADD A SECTION OF STRING
! PAST ITS END.
      IF(IP<=3)THEN
       IF(IP<=0)MSTR1=''
       IF(IP==1)MSTR1='.'
       IF(IP==2)MSTR1='..'
       IF(IP==3)MSTR1='...'
       IL=0
      ENDIF
      IF(IL>IP)THEN
       IL2=IP-3
       IF(ABS(RP-2.0)<0.01)THEN
        MSTR1=MSTR1(:IL2)//"..."
       ELSE
        MSTR1='...'//MSTR1(IL-IL2+1:)
       ENDIF
      ENDIF
      GOTO 999
!
160   CONTINUE
      IL=LEN_TRIM(MSTR1)
      DO I=1,IL
       IBAD=0
       IF(MSTR1(I:I)=='/')IBAD=1
       IF(MSTR1(I:I)=='\')IBAD=1
       IF(MSTR1(I:I)==' ')IBAD=1
       IF(MSTR1(I:I)=="'")IBAD=1
       IF(MSTR1(I:I)=='"')IBAD=1
       IF(MSTR1(I:I)=='`')IBAD=1
       IF(MSTR1(I:I)==':')IBAD=1
       IF(MSTR1(I:I)==';')IBAD=1
       IF(MSTR1(I:I)=='|')IBAD=1
       IF(MSTR1(I:I)=='*')IBAD=1
       IF(IBAD==1)MSTR1(I:I)='_'
      ENDDO
      GOTO 999
!
 999  RETURN
      END subroutine
      
!====================================== INPARAN
      function inparan(str,ispot,imode)
! Is the place, ISPOT, in string STR inside of parantheses?
! IMODE - 1 - return a 0 if not in parans. If we are in parans,
!             return the depth.
!         2 - return the first spot in the string that is NOT inside of
!             parans, starting from ISPOT. This will be when the depth
!             decreases. That will allow me to start from anywhere in
!             the string. ABC(123)=5 will let me start from the "A" or
!             the C.
      character str*(*)
      iret=-1
      idepth=0
      idec=0
      icd=0
      il=len_trim(str)
      do i=1,il
       if(imode==2.and.i==ispot)idec=1
       if(idec==1)icd=idepth
       if(str(i:i)=="(")idepth=idepth+1
       if(imode==1.and.i==ispot)exit
       if(str(i:i)==")")idepth=idepth-1
       if(idec==1.and.idepth<icd)then
        idepth=i+1
        exit
       endif
      enddo
99    continue
      inparan=idepth
      return
      end function
      
!======================== FILEPATHPARSE
      subroutine filepathparse(fl,drivelet,path,fname)      
!  Chop a filename into individual pieces.
      character fl*(*),drivelet,path*1000,fname*100
      drivelet="?"
      path="?"
      fname="?"
! Find the last slash
      ix1=index(fl,"/",.TRUE.); ix2=index(fl,"\",.TRUE.)
      ix=1; if(ix1>ix)ix=ix1; if(ix2>ix)ix=ix2
      ixx=ix
! File name is anything after last slash.
      fname=fl(ix+1:)
! Find first slash if any
      ix1=index(fl,"/"); ix2=index(fl,"\")
      ix=1; if(ix1>0)ix=ix1; if(ix2>0.and.ix2>ix)ix=ix2
! Path is anything between first and last slash      
      path=fl(ix:ixx)
! If entire path starts with . or if file name equals entire
! path, I have no idea what the drive letter is.
      if(fl(1:1)==".")goto 99
      if(fl==fname)goto 99
! Drive letter is first character of entire.
      drivelet=fl(1:1)
! If this is a cygwin path, drive letter is 9th character.
      if(fl(:8)=="/cygwin/")drivelet=fl(9:9)
99    continue      
      return
      end subroutine
end module
