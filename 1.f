      program main
      use fileutilmod
      use othermod
      use cmdlinemod
      use globalmod
      use avldiskmod
      use dflib
C stuffB
      
      character speedstr*100,speedback*10
      
c      call quicksorttest
      
      ii=5
c      i=PRINTPOOP(ii,"./unity/geo1/umqaabab.dat")

c      TYPE (FILE$INFO) info
c      character files*80

      call fileutilinit
      call loaddebug
      call getcmdline
      call setcglobs_c(igquiet,iqperf)
C test comment for fun ad profit2
      
      
      if(igspeed>0)then
C new comment to mess with ya!
       call delfile("output.txt",1)
       speedstr="This is the speed string!"
       if(igspeed==1)iff=ifopen("output.txt","w")
       do i=1,1000
        if(igspeed==1)call speedtest(speedstr,iff)
        if(igspeed==2)call speedtest_c(trim(speedstr)//achar(0),0,
     *  speedback)
       enddo
       if(igspeed==1)call fclose(iff)
       call enditall("Speedtest!")
      endif
      
      igflog=ifopen("masterrebuild.log","w")
      
      call sysdate
      call getcid
      call openavl
      call avldir(gid//"QFILNM"); fqfilnmloc=gavlfull
      call logit("QFILNM: "//trim(gavlfull),1)
      call avldir(gid//"QMASTR"); fqmastrloc=gavlfull
      call logit("QMASTR: "//trim(gavlfull),1)
      call getlastq
      ilastq=iqtol(lastq)
      call geoparse(3,iqcnt)
      call getlastf
      call smitef
      if(lastq/="ZZZZZ")then
       call geoparse(2,iqcnt)
       call geoparse(4,iqcnt)
      endif
      
c      call sortpartname(iqcnt)
c      call enditall("bubblesort")
      call sortpartname_q(iqcnt)
c      call buildffiles(iqcnt)

      call fclose(igflog)
      call buildffiles_c(iqcnt,gid,gext,lastf)
      igflog=ifopen("masterrebuild.log","a")
      
      call buildqmastr
      call updateqfilnm(1,lastq)
      call updateqfilnm(5,lastf)
      
c      ih=FILE$FIRST
c      ih=FILE$LAST
c      ih=FILE$ERROR
c      
c      ilength = GETSTRQQ(files)
c      ihandle = FILE$FIRST
c      do
c       ilength=GETFILEINFOQQ(files, info, ihandle)
c       if(ihandle<0)exit
c      enddo

      call logit("Complete!",1)
      call logit("",1)
      call fclose(igflog)

      end

C======================== GETCID
      subroutine getcid
      use globalmod
      use fileutilmod
      
      iff=ifopen("IDFILE.DAT","r")
      call nextline(iff,ieof)
      call nextline(iff,ieof)
      gid=fileline(6:7)
      call fclose(iff)
      call logit("Cust ID: "//gid,1)
      return
      end
      
C======================= SYSDATE
      subroutine sysdate
      use globalmod
      character yy*2,mm*2,dd*2
      integer iv(8)
      call logit("Getting system date...",1)
      call DATE_AND_TIME(values=iv)
      iy2=mod(iv(1),10)
      iy1=(mod(iv(1),100)-iy2)/10
      im2=mod(iv(2),10)
      im1=(mod(iv(2),100)-im2)/10
      id2=mod(iv(3),10)
      id1=(mod(iv(3),100)-iy2)/10
      yymmdd(1:1)=achar(iy1+48)
      yymmdd(2:2)=achar(iy2+48)
      yymmdd(3:3)=achar(im1+48)
      yymmdd(4:4)=achar(im2+48)
      yymmdd(5:5)=achar(id1+48)
      yymmdd(6:6)=achar(id2+48)

      return
      end
      
C=========================== GETLASTQ
      subroutine getlastq
      use avldiskmod
      use globalmod
      use strutilmod
      use othermod
      character str*6,flast*100
      logical lex
      
      call logit("Finding last Q-file...",1)

C The last qfile sequence could be listed in the AVLDISK as ZZZZZ, which
C is an EXCLUSIVE match, so I use ZZZZY to make sure it's found
      str="QZZZZY"
      call avldir(gid//str)
      igeo=igeonum(gavldir)
      ilastavlgeo=igeo
C See how many legit qfiles I have in the last geo directory. If there aren't
C any, I need to check the previous GEO dir and so on until I find the one that
C contains the last existing qfile.
10    continue
      ifcnt=0
      inquire(directory=gavldir,exist=lex)
c      if(.not.lex)call enditall("Missing directory: "//trim(gavldir))
      if(lex)call lastqfile(trim(gavldir)//gid//"Q"//"*"//
     *gext,flast,ifcnt)
      if(ifcnt==0)then
       ix=strfind("GEO",gavldir,ib,ie,1,-1)
       igeo=igeo-1
       if(igeo<=0)then
! I used to end if I didn't find at least SOME Q-file in the
! GEO1 directory, but I want it instead to build a valid, empty
! QMASTR and F file.
!        call enditall("No valid GEO directory found!")
        igeo=1
        str=num2stri(igeo)
        gavldir=gavldir(:ie)//trim(str)//"/"
        iii=1
        goto 20
       endif
       str=num2stri(igeo)
       gavldir=gavldir(:ie)//trim(str)//"/"
       goto 10
      endif
      
20    continue
      
      lastq=flast(4:8)
      if(ifcnt==0)then
       lastq="ZZZZZ"
       call logit("No valid Q-files were found!"//
     * " Creating blank library.",0)
      endif
      ilastgeo=igeo
      if(igeo>0)then
       ix=strfind("GEO",gavldir,ib,ie,1,-1)
       geopath=gavldir(1:ie)
      else
       geopath=gavldir
      endif      
      call logit("  Last Q-file: "//trim(lastq),0)

       
      return
      end
      
C======================= LASTQFILE
      subroutine lastqfile(fdir,flast,ifcnt)
      use globalmod
      use othermod
      use dflib
      use avldiskmod
      character fdir*(*),flast*(*)
      TYPE (FILE$INFO) info
      ifcnt=0
      il=len_trim(gext)
      ih=file$first
      do
       il=getfileinfoqq(fdir, info, ih)
       if(ih<0)exit
       if(il>0)then
        if(info%name(9:9+il-1)/=gext)cycle
       endif
       if(info%name(1:8)==gid//"QFILNM")cycle
       if(info%name(1:8)==gid//"QMASTR")cycle
       flast=info%name
       ifcnt=ifcnt+1
      enddo
      return
      end

C======================= DIRINFO
      subroutine dirinfo
      use globalmod
      use avldiskmod
      use dflib
      
      TYPE (FILE$INFO) info
      character files*80

      ihandle = FILE$FIRST
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)
      ihandle = FILE$FIRST
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)
      ilength=GETFILEINFOQQ("./unity/*.DAT", info, ihandle)

      return
      end
      
C========================= IGEONUM
      function igeonum(fstr)
      use strutilmod
      character fstr*(*)
      call columnparse(fstr,"/",1,0,0)
      il=len_trim(gparsecol(gparsecolcnt))
      iff=0
      do i=1,il
       if(isnum(gparsecol(gparsecolcnt)(i:i),0,0)==1)then
        iff=1
        exit
       endif
      enddo
      if(iff==0)then
       igeonum=-1
      else
       call str2num(gparsecol(gparsecolcnt),igeonum,rval,1)
      endif
      return
      end
      
C=========================== GETLASTF
      subroutine getlastf
      use avldiskmod
      use globalmod
      use strutilmod
      use othermod
      character str*6,flast*100
      logical lex
      
      call logit("Finding last F-file...",1)

C The last ffile sequence could be listed in the AVLDISK as ZZZZZ, which
C is an EXCLUSIVE match, so I use ZZZZY to make sure it's found
      str="FZZZZY"
      call avldir(gid//str)
      igeo=igeonum(gavldir)
C See how many legit ffiles I have in the last geo directory. If there aren't
C any, I need to check the previous GEO dir and so on until I find the one that
C contains the last existing ffile.
10    continue
      inquire(directory=gavldir,exist=lex)
      if(.not.lex)call enditall("Missing directory: "//trim(gavldir))
      call lastffile(trim(gavldir)//gid//"F"//"*"//gext,flast,ifcnt)
      if(ifcnt==0)then
       ix=strfind("GEO",gavldir,ib,ie,1,-1)
       igeo=igeo-1
       if(igeo<=0)then
        lastf="@@@@@"
        goto 20
       endif
       str=num2stri(igeo)
       gavldir=gavldir(:ie)//trim(str)//"/"
       goto 10
      endif
      
      lastf=flast(4:8)
      call logit("  Last F-file: "//trim(lastf),0)
20    continue       
      return
      end
      
C======================= LASTFFILE
      subroutine lastffile(fdir,flast,ifcnt)
      use globalmod
      use othermod
      use dflib
      use strutilmod
      character fdir*(*),flast*(*),a5*5
      TYPE (FILE$INFO) info
      ifcnt=0
      ih=file$first
      do
       il=getfileinfoqq(fdir, info, ih)
       if(ih<0)exit
       a5=info%name(4:8)
       call ucase(a5)
       if(a5=="ILFLG")cycle
       ifcnt=ifcnt+1
       flast=info%name
      enddo
      return
      end

C========================= BUILDQMASTR
      subroutine buildqmastr
      use globalmod
      use avldiskmod
      use fileutilmod
      character curf*5,ffil*8,firstpart*80

      call logit("Building the QMASTER file...",1)

      call delfile(fqmastrloc,1)
      ifo=ifopen(fqmastrloc,"w")
      ift=ifopen("tmp.tmp","w")
      call writeline("PART DIRECTORY",ifo)
1001  format ("*",i8,"  ",a40,a8)       
      curf="AAAAA"
      itp=0
      do
       ffil=gid//"F"//curf
       call avldir(ffil)
       if(ifilchk(gavlfull,"r")==0)goto 10
       iff=ifopen(gavlfull,"r")
       icnt=0; firstpart=""; ifirst=1
       do
        call nextline(iff,ieof)
        if(ieof/=0)exit
        if(fileline(1:1)=="*")then
         icnt=icnt+1
         if(ifirst==1)then; firstpart=fileline(2:41); ifirst=0; endif
        endif
       enddo
       itp=itp+icnt
       write(ift,1001),icnt,firstpart,gid//"F"//curf
       call fclose(iff)
10     continue
       if(curf==lastf)exit
       call alphaplus1(curf)
      enddo

1000  format (i8," TOTAL PARTS")       
      write(ifo,1000),itp                             
      write(ift,"(a)"),"#"
      call fclose(ift)
      call fclose(ifo)
      call fileappend("tmp.tmp",fqmastrloc,1)
      
      return
      end
      
C====================== ALPHAPLUS1
      subroutine alphaplus1(cur)
      use othermod
      character cur*5
      iii=0
      ic=5
      do
       iv=ichar(cur(ic:ic))
       iv=iv+1
       if(iv<91)then
        cur(ic:ic)=achar(iv)
        exit
       else
        cur(ic:ic)="A"
       endif
       ic=ic-1
       if(ic==0)call enditall("Ran out of letters!")
      enddo
      return
      end
      
C========================= SMITEF
      subroutine smitef
      use globalmod
      use avldiskmod
      use fileutilmod
      character curf*5,fil8*8

      call logit("Removing existing F-files...",1)

      if(lastf=="@@@@@")goto 10
      curf="AAAAA"
      do
       fil8=gid//"F"//curf
       call avldir(fil8)
       if(ifilchk(gavlfull,"r")==1)then
        call logit("  Deleting "//trim(gavlfull)//"...",0)
        call delfile(gavlfull,1)
       endif
       if(curf==lastf)exit
       call alphaplus1(curf)
      enddo
10    continue
      return
      end

C========================= GEOPARSE
      subroutine geoparse(imode,iqcnt)
C IMODE  2 - make sure Q-files are in the correct GEO directory.
C        3 - make sure all of the geo dirs exist
C        4 - scan Q-files for errors and get list of Q sequences and part names
C        5 - scan Q-files for errors but to NOT make list of Q sequences and part names
      use globalmod
      use avldiskmod
      use strutilmod
      use othermod
      use dflib
      use fileutilmod
      TYPE (FILE$INFO) info
      character curpath*200,curq*5,curpathl*200,qseq*6,pname*41,str*500
      logical lex

      iqcnt=0
      if(imode==2)call logit("Putting Q-files in correct GEO dirs...",1)
      if(imode==3)call logit("Making sure all GEO dirs exist...",1)
      if(imode==4.or.imode==5)then
       call logit("Checking Q-files for errors...",1)
       igbadcnt=0
       igprecnt=0
       igsancnt=0
       if(imode==4)then
        call delfile("qorph.txt",1)
        ifq=ifopen("qorph.txt","b")
        call delfile("porph.txt",1)
        ifp=ifopen("porph.txt","b")
       endif
      endif

C move through the geo directories and get Q-file info
      ibad=0
      icurgeo=1
      igextl=len_trim(gext)
      if(ilastgeo<0)icurgeo=-1
      do
       curpath=trim(geopath)//trim(num2stri(icurgeo))//"/"
       if(ilastgeo<0)curpath=geopath
       
       if(imode==3)then
        if(icurgeo>ilastavlgeo)exit
        inquire(directory=curpath,exist=lex)
        if(.not.lex)then
         call logit("  Creating missing directory "//
     *   trim(curpath)//"...",0)
         curpathl=curpath
         call lcase(curpathl)
         call createdir(curpathl,0)
        endif
        goto 20
       endif
       ilper=0
       ih=file$first
       do
        il=getfileinfoqq(trim(curpath)//gid//"Q*"//gext, info, ih)
        if(ih<0)exit
C make sure this is a legit file name.
C if GEXT is used, then i better have a IDQAAAAA.DAT
C format to my file name.
        curq="?????"
        if(igextl>0)then
         if(info%name(9:9+igextl-1)/=gext)goto 50
        endif
        curq=info%name(4:8)
        if(curq=="FILNM")cycle
        if(curq=="MASTR")cycle
        if(imode==4.or.imode==5)then
         call qfileerrorcheck(trim(curpath)//gid//"Q"//curq//gext,
     *   ierr,ipre,qseq,pname)
         igprecnt=igprecnt+ipre
C If this is a preshape, see if I need to fix the date
         if(ipre==1)then
          if(ierr==7)call fixdate(trim(curpath)//gid//"Q"//curq//gext,
     *    gid//"Q"//curq//gext)
          goto 50
         endif
         if(ierr>0)then
          if(ierr==5)then
           igsancnt=igsancnt+1
          else
           call logit(gid//"Q"//curq//gext// " is bad!",0)
           igbadcnt=igbadcnt+1
           if(igbadcnt==1)call createdir("q_bad",0)
           call renamefile(trim(curpath)//gid//"Q"//curq//gext,
     *     "./q_bad/"//gid//"Q"//curq//gext,0)
          endif
          goto 50
         endif
        endif
C at this point I have a normal, correct Q-file
        iqcnt=iqcnt+1
        iqnum=iqtol(curq)
        
        if(imode==2)then
C  ---- progress listing, every percentage point
         icper=((iqnum*100.0)/real(ilastq))+.5
         if(icper>ilper)then
          ilper=icper
          write(str,"(2x,a,i9,'%',i11,a)"),curq//"  /  "//lastq,icper,
     *    iqcnt,"  Q-files found"
          call logit(str,0)
         endif
C  ---- make sure Q-file are in correct GEO direcotries
         call avldir(info%name)
         if(curpath/=gavldir)then
          call logit(gid//"Q"//curq//gext//" moved to "//
     *    trim(gavldir),0)
          call filemove(trim(curpath)//gid//"Q"//curq//gext,
     *    trim(gavldir)//gid//"Q"//curq//gext)
         endif
        elseif(imode==4)then
C  ---- Error check Q-files and get name and Q sequence
         call putq(qseq,iqcnt,ifq)
         call putp(pname,iqcnt,ifp)
C  ---- progress listing, every percentage point
         icper=((iqnum*100.0)/real(ilastq))+.5
         if(icper>ilper)then
          ilper=icper
          write(str,"(2x,a,i9,'%',i11,a)"),curq//"  /  "//lastq,icper,
     *    iqcnt,"  Q-files checked"
          call logit(str,0)
         endif
        endif
50      continue 
        if(curq==lastq)goto 10
       enddo
       if(igeo==ilastgeo)goto 10
20     continue       
       icurgeo=icurgeo+1
       if(ilastgeo==-1)exit
      enddo
10    continue
      if(imode==4)then; call fclose(ifq); call fclose(ifp); endif
      if(ibad==1)then
       call logit("  Errors, so quitting.",1)
       call enditall("")
      endif
      return
      end
      
C===================== LOGIT
      subroutine logit(str,iline)
      use globalmod
      use fileutilmod
      use cmdlinemod
      character str*(*)
      if(iline==1)then
       call writeline("",igflog)
       if(igquiet==0)print "(a)",""
      endif
      
      call writeline(str,igflog)
      if(igquiet==0)print "(a)",trim(str)
      return
      end
      
C======================== QFILEERRORCHECK
      subroutine qfileerrorcheck(fl,ierr,ipreshape,qseq,pname)
C IERR  1 - no F-file information found
C       2 - part name line was bad
C       3 - F-file section has too few lines
C       4 - F-file section has too many lines
C       5 - this Qfile is for a SAN

c      use fileutilmod
c      use strutilmod
      use othermod
      character fl*(*),c2*2,cf*2,cs*2,qseq*6,pname*41
      ierr=0
c      iff=ifopen(fl,"R")
c      iw=gfilestat(iff)%wholeptr
c      iloc=gfilestat(iff)%size-1
c      cf=achar(10)//"*"
c      cs=achar(10)//"@"
cc      ix=strfind(cf,wholefile(iw)%str,ib1,ie1,1,-1)
cc      ix=strfind(cs,wholefile(iw)%str,ib2,ie2,1,-1)
c      do
c       c2=wholefile(iw)%str(iloc:iloc+1)
cC If I find an x10 followed by @, this is a SAN
c      if(wholefile(iw)%str(iloc:iloc+1)==cs)then; ierr=5; goto 20; endif
cC If I find an x10 followed by asterisk, this is the part info
c       if(wholefile(iw)%str(iloc:iloc+1)==cf)exit
c       iloc=iloc-1
cC If reach the start of the file, bad Q-file
c       if(iloc==0)then; ierr=1; goto 20; endif
c      enddo
C at this point, I found the beginning of the part info
      call finfo_c(trim(fl)//achar(0),ierr,ipreshape,qseq,pname)
      if(ierr==99)call enditall("Something HORRIBLE happened"//
     *" (permissions?) when trying to load "//fl)
      return
      end
      
C=========================== SPEEDTEST
      subroutine speedtest(str,iff)
      use fileutilmod
      character str*(*)
      write(iff,"(a)"),trim(str)
      return
      end
      
C========================== PUTQ
      subroutine putq(qseq,islot,ifq)
      use fileutilmod
      character qseq*6
      qseq(6:6)=" "
      ib=((islot-1)*6)+1
      call putstring(ifq,ib,6,qseq)
      return
      end

C========================== PUTP
      subroutine putp(pname,islot,ifp)
      use fileutilmod
      character pname*41
      pname(41:41)=" "
      ib=((islot-1)*41)+1
      call putstring(ifp,ib,41,pname)
      return
      end
      
C========================== GETP
      subroutine getp(pname,islot,ifp)
      use fileutilmod
      character pname*41
      ib=((islot-1)*41)+1
      call getstring(ifp,ib,41,pname)
      return
      end
      
C========================== GETQ
      subroutine getq(qseq,islot,ifp)
      use fileutilmod
      character qseq*6
      ib=((islot-1)*6)+1
      call getstring(ifp,ib,6,qseq)
      return
      end
      
C====================== SORTPARTNAME
      subroutine sortpartname(iqcnt)
      use fileutilmod
      use avldiskmod
      use globalmod
      character*41 p1,p2,pt
      character*6 q1,q2,qt
      character qsrc*100
      logical lswapit
      call logit("Sorting part names...",1)
      idupecnt=0
      ifp=ifopen("porph.txt","B")
      ifq=ifopen("qorph.txt","B")
      do i1=1,iqcnt-1
       do i2=i1+1,iqcnt
        call getp(p1,i1,ifp); p1(41:41)=" "
        call getp(p2,i2,ifp); p2(41:41)=" "
        if(p2<p1)then
         call getq(q1,i1,ifq)
         call getq(q2,i2,ifq)
         call putp(p2,i1,ifp)
         call putp(p1,i2,ifp)
         call putq(q2,i1,ifq)
         call putq(q1,i2,ifq)
        elseif(p1==p2)then
         idupecnt=idupecnt+1
         if(idupecnt==1)call createdir("q_dups",0)
         call movedupe(p1,p2,i1,i2,ifp,ifq)
c         do i=1,41; p2(i:i)="~"; enddo
c         call putp(pname2,i2,ifp)
        endif
       enddo
      enddo
      
      call updatebwhole(ifp)
      call updatebwhole(ifq)
      call fclose(ifp)
      call fclose(ifq)
      return
      end
      
C=============================== CREATEDIR
      subroutine createdir(dn,imode)
C IMODE 0 - if DN already exists, it's NOT an error
C       1 - if DN already exists, it IS an error
      use dflib
      use othermod
      character dn*(*)
      ih=makedirqq(dn)
      if(ih<=0)then
       ih=getlasterrorqq()
       if(ih==err$exist.and.imode==0)goto 10
       call enditalli("Dir create failed! "//trim(dn),ih)
10     continue       
      endif
      return
      end
      
C=============================== RENAMEFILE
      subroutine renamefile(src,dst,imode)
C IMODE 0 - if DST exists, remove it first so file move doesn't error
C       1 - if DST exists, error the file move
      use dflib
      use fileutilmod
      use othermod
      character src*(*),dst*(*)
      if(imode==0)call delfile(dst,1)
      
      ih=renamefileqq(src,dst)
      if(ih<=0)then
       ih=getlasterrorqq()
       if(ih==err$exist)call enditall("File "//trim(dst)//" exists!")
       call enditalli("File move error! ",ih)
      endif
      
      return
      end

C===================== QUICKSORTTEST
      subroutine quicksorttest
      use othermod
      integer arr(10)
      print *,"Hi"
      arr(1)=5
      arr(2)=2
      arr(3)=8
      arr(4)=6
      arr(5)=4
      arr(6)=10
      arr(7)=7
      arr(8)=9
      arr(9)=1
      arr(10)=5
      call quicksort2(arr,1,10,10)
      call enditall("fart")
      return
      end
      
C======================= QUICKSORT2
      recursive subroutine quicksort2(arr,ist,ied,itot)
C this is a quick sort algorithm that will handle same values
      use othermod
      integer arr(itot)
      ipiv=arr(ist)
      i1=ist
      i2=ied+1
      do
       do
        i1=i1+1
        if(arr(i1)==ipiv)then
         iii=0
        endif
        if(arr(i1)>=ipiv)exit
        if(i1==ied)exit
       enddo
       do
        i2=i2-1
        if(arr(i2)==ipiv.and.i2>ist)then
         arr(i2)=999
         iii=0
        endif
        if(arr(i2)<=ipiv)exit
        if(i2==ist)exit
       enddo
       if(i1>=i2)exit
c       if(arr(i1)==ipiv)i1=i1+1
c       if(i1==i2)exit
       call swapi(arr(i1),arr(i2))
      enddo
      call swapi(arr(ist),arr(i2))
      if(i2-1>ist)call quicksort2(arr,ist,i2-1,itot)
      if(i2+1<ied)call quicksort2(arr,i2+1,ied,itot)
20    continue      
      return
      end

C======================= QUICKSORT
      recursive subroutine quicksort(arr,ist,ied,itot)
C this is a quick sort algorithm that will handle same values
      use othermod
      integer arr(itot)
      ipiv=arr(ist)
      i1=ist
      i2=ied+1
      do
       do
        i1=i1+1
        if(arr(i1)>=ipiv)exit
        if(i1==ied)exit
       enddo
       do
        i2=i2-1
        if(arr(i2)<=ipiv)exit
        if(i2==ist)exit
       enddo
       if(i1>=i2)exit
c       if(arr(i1)==ipiv)i1=i1+1
c       if(i1==i2)exit
       call swapi(arr(i1),arr(i2))
      enddo
      call swapi(arr(ist),arr(i2))
      if(i2-1>ist)call quicksort(arr,ist,i2-1,itot)
      if(i2+1<ied)call quicksort(arr,i2+1,ied,itot)
20    continue      
      return
      end
      
C================================= SORTPARTNAME-QUICK
      recursive subroutine sortpartname_quick(ist,ied,itot,ifp,ifq)
      character ppiv*41,p1*41,p2*41,q1*6,q2*6,ptmp*41
      call getp(ppiv,ist,ifp)
      ifirst=1
      i1=ist
      i2=ied+1
      do
       do
        i1=i1+1
        call getp(p1,i1,ifp)
         if(p1==ppiv.and.i1>ist)then
         if(ifirst==1)then;ifirst=0;call createdir("q_dups",0); endif
         call movedupe2(ppiv,p1,ist,i1,ifp,ifq)
        endif
        if(p1>=ppiv)exit
        if(i1==ied)exit
       enddo
       do
        i2=i2-1
        call getp(p2,i2,ifp)
        if(p2==ppiv.and.i2>ist)then
         call movedupe2(p1,p2,i1,i2,ifp,ifq)
        endif
        if(p2<=ppiv)exit
        if(i2==ist)exit
       enddo
       if(i1>=i2)exit
       call getq(q1,i1,ifq); call getq(q2,i2,ifq)
       call putp(p2,i1,ifp); call putp(p1,i2,ifp)
       call putq(q2,i1,ifq); call putq(q1,i2,ifq)
      enddo      
      call getq(q1,ist,ifq); call getq(q2,i2,ifq)
      call putq(q2,ist,ifq); call putq(q1,i2,ifq)
      call putp(p2,ist,ifp); call putp(ppiv,i2,ifp)
      if(i2-1>ist)call sortpartname_quick(ist,i2-1,itot,ifp,ifq)
      if(i2+1<ied)call sortpartname_quick(i2+1,ied,itot,ifp,ifq)       
      return
      end
      
C============================= MOVEDUPE
      subroutine movedupe(p1,p2,i1,i2,ifp,ifq)
      use avldiskmod
      use globalmod
      character p1*41,p2*41,q1*6,q2*6,qsrc*100
      logical lswapit
C I have a duplicate part name. Get the Q sequences       
         call getq(q1,i1,ifq); q1(6:6)=" "
         call getq(q2,i2,ifq); q2(6:6)=" "
C If the first Q sequence is "newer", I want to keep this
C one and zero out the second one.
         lswapit=.FALSE.
         if(q2>q1)lswapit=.TRUE.
         if(lswapit)then
          qsrc=gid//"Q"//trim(q1)//gext
          call avldir(qsrc)
          call logit("  Duplicate: Kept "//q2(1:5)//", moved "//
     *    q1(1:5)//". "//p1,0)
          q1=q2
         else
          qsrc=gid//"Q"//trim(q2)//gext
          call avldir(qsrc)
          call logit("  Duplicate: Kept "//q1(1:5)//", moved "//
     *    q2//". "//p1,0)
         endif
         call renamefile(gavlfull,"./q_dups/"//qsrc,0)
         do i=1,6; q2(i:i)="@"; enddo
         do i=1,41; p2(i:i)="~"; enddo
         call putp(p2,i2,ifp)
         call putq(q2,i2,ifq)
         if(lswapit)call putq(q1,i1,ifq);
      return
      end
      
C============================== SORTPARTNAME_Q
      subroutine sortpartname_q(iqcnt)
      use fileutilmod
      use globalmod
      use othermod
      character q*6
      ifp=ifopen("porph.txt","B")
      ifq=ifopen("qorph.txt","B")
      call logit("Sorting part names...",1)
      ied=iqcnt
      itot=iqcnt
      if(iqcnt>1)call sortpartname_quick(1,ied,itot,ifp,ifq)
      call updatebwhole(ifp)
      call updatebwhole(ifq)
c vvvvvvvvvvvvv
c This section used to update IQCNT based on how many duplicate parts I
c had. However, while the process of sorting does move part names and
c Q-file sequences, it will NOT move duplicate parts to the end of QORPH
c and PORPH but will instead replace the part name with "~" and the
c Q sequence with "@". I need to be smart enough to skip over those
c when I build the F-files.
c     id=0
c     do i=iqcnt,1,-1
c      call getq(q,i,ifq)
c      if(q(1:5)/="@@@@@")exit
c      id=id+1
c     enddo
c     iqcnt=iqcnt-id
! ^^^^^^^^^^^^^
      call fclose(ifp)
      call fclose(ifq)
c      call enditall("quicksort")
      return
      end
      
C============================= MOVEDUPE2
      subroutine movedupe2(p1,p2,i1,i2,ifp,ifq)
C this is a special version for use ONLY by a quick sort. P1 MUST remain
C the part name and p2 is the one that changes. Therefore, I also want the
C Qfile sequece to keep to align with p1.
      use avldiskmod
      use globalmod
      character p1*41,p2*41,q1*6,q2*6,qsrc*100
      logical lswapit
C I have a duplicate part name. Get the Q sequences       
         call getq(q1,i1,ifq); q1(6:6)=" "
         call getq(q2,i2,ifq); q2(6:6)=" "
C if either of the Q files has already been tagged a dupe,
C then I don't need to move anything.
         if(q1(1:5)=="@@@@@".or.q2(1:5)=="@@@@@")goto 99
C If the first Q sequence is "newer", I want to keep this
C one and zero out the second one.
         lswapit=.FALSE.
         if(q2>q1)lswapit=.TRUE.
         if(lswapit)then
          qsrc=gid//"Q"//trim(q1)//gext
          call avldir(qsrc)
          call logit("  Duplicate: Kept "//q2(1:5)//", moved "//
     *    q1(1:5)//". "//p1,0)
          q1=q2
         else
          qsrc=gid//"Q"//trim(q2)//gext
          call avldir(qsrc)
          call logit("  Duplicate: Kept "//q1(1:5)//", moved "//
     *    q2//". "//p1,0)
         endif
         call renamefile(gavlfull,"./q_dups/"//qsrc,0)
         do i=1,6; q2(i:i)="@"; enddo
         do i=1,41; p2(i:i)="~"; enddo
         call putp(p2,i2,ifp)
         call putq(q2,i2,ifq)
         if(lswapit)call putq(q1,i1,ifq);
99    continue 
      return
      end

C=================================== BUILDFFILES
      subroutine buildffiles(iqcnt)
      use fileutilmod
      use globalmod
      use avldiskmod
      use cmdlinemod
      character q*6,cur*5,str*50
      call logit("Creating F-files...",1)
      cur="AAAAA"
      ifq=ifopen("qorph.txt","B")
      ic=0
      str=gid//"F"//cur//gext
      call avldir(str)
      ifo=ifopen(gavlfull,"w")
      call logit("  Writing "//trim(str)//"...",0)
      do i=1,iqcnt
       ic=ic+1
       call getq(q,i,ifq)
       str=gid//"Q"//q(1:5)//gext
       call avldir(str)
       ifqi=ifopen(gavlfull,"R")
       iloc=gfilestat(ifqi)%size
       iw=iwholeslot(ifqi)
       do
        iloc=iloc-1
        if(wholefile(iw)%str(iloc:iloc+1)==achar(10)//"*")then
         call wholepos(ifqi,iloc)
         exit
        endif
       enddo
       do
        call nextline(ifqi,ieof)
        if(ieof/=0)exit
        write(ifo,"(a)"),trim(fileline)
       enddo
       call fclose(ifqi)
       if(ic==iqperf.and.i<iqcnt)then
        ic=0
        write(ifo,"(a)"),"#"
        call fclose(ifo)
        call alphaplus1(cur)
        str=gid//"F"//cur//gext
        call avldir(str)
        ifo=ifopen(gavlfull,"w")
        call logit("  Writing "//trim(str)//"...",0)
       endif
      enddo
      write(ifo,"(a)"),"#"
      call fclose(ifo)
      call fclose(ifq)
      return
      end
      
C=============================== IQTOL
      function iqtol(q)
      character q*5
      iret=(ichar(q(1:1))-65)*(26**4)
      iret=iret+((ichar(q(2:2))-65)*(26**3))
      iret=iret+((ichar(q(3:3))-65)*(26**2))
      iret=iret+((ichar(q(4:4))-65)*(26**1))
      iret=iret+((ichar(q(5:5))-65)*(26**0))
      iqtol=iret
      return
      end
      
C============================ UPDATEQFILNM
      subroutine updateqfilnm(iline,lq)
      use avldiskmod
      use fileutilmod
      use globalmod
      character lq*5
      call logit("Updating QFILNM with "//lq//"...",1)
      iff=ifopen(fqfilnmloc,"r")
      ifo=ifopen("tmp.out","w")
      ilc=0
      do
       call nextline(iff,ieof)
       if(ieof/=0)exit
       ilc=ilc+1
       if(ilc==iline)fileline(1:5)=lq
       write(ifo,"(a)"),trim(fileline)
      enddo
      call fclose(iff)
      call fclose(ifo)
      call filemove("tmp.out",fqfilnmloc)
      return
      end

C ============================= FIXDATE
      subroutine fixdate(qf,qf2)
      use fileutilmod
      use globalmod
      character qf*(*),qf2*(*)
      
      call logit("  Repairing date of pre-tooled shape "//
     *trim(qf2)//"...",0)
      
      ifq=ifopen(qf,"r")
      ifo=ifopen("tmp.txt","w")
      do
       call nextline(ifq,ieof)
       if(ieof/=0)exit
       if(fileline(1:1)=="*")then
        write(ifo,"(a)"),trim(fileline)
        call nextline(ifq,ieof); write(ifo,"(a)"),trim(fileline)
        call nextline(ifq,ieof); write(ifo,"(a)"),trim(fileline)
        call nextline(ifq,ieof)
        ibad=0
        do i=13,17
         ic=ichar(fileline(i:i))
         if(ic<48.or.ic>57)then; ibad=1; exit; endif
        enddo
        if(ibad==1)fileline(12:17)=yymmdd
        ibad=0
        do i=19,24
         ic=ichar(fileline(i:i))
         if(ic<48.or.ic>57)then; ibad=1; exit; endif
        enddo
        if(ibad==1)fileline(19:24)=yymmdd
        write(ifo,"(a)"),trim(fileline)
       else
        write(ifo,"(a)"),trim(fileline)
       endif
      enddo
      call fclose(ifq)
      call fclose(ifo)
      call filemove("tmp.txt",qf);
      
      return
      end
