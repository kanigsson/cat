with Interfaces.C; use Interfaces.C;
package Const_H with
  SPARK_Mode
is
   ADA_F_DUPFD : constant Int := 0;
   ADA_F_DUPFD_CLOEXEC : constant Int := 1030;
   ADA_F_GETFD : constant Int := 1;
   ADA_F_SETFD : constant Int := 2;
   ADA_F_GETFL : constant Int := 3;
   ADA_F_SETFL : constant Int := 4;
   ADA_F_GETLK : constant Int := 5;
   ADA_F_SETLK : constant Int := 6;
   ADA_F_SETLKW : constant Int := 7;
   ADA_F_GETOWN : constant Int := 9;
   ADA_F_SETOWN : constant Int := 8;
   ADA_FD_CLOEXEC : constant Int := 1;
   ADA_F_RDLCK : constant Int := 0;
   ADA_F_UNLCK : constant Int := 2;
   ADA_F_WRLCK : constant Int := 1;
   ADA_O_CLOEXEC : constant Int := 02000000;
   ADA_O_CREAT : constant Int := 0100;
   ADA_O_DIRECTORY : constant Int := 0200000;
   ADA_O_EXCL : constant Int := 0200;
   ADA_O_NOCTTY : constant Int := 0400;
   ADA_O_NOFOLLOW : constant Int := 0400000;
   ADA_O_TRUNC : constant Int := 01000;
   ADA_O_APPEND : constant Int := 02000;
   ADA_O_DSYNC : constant Int := 010000;
   ADA_O_NONBLOCK : constant Int := 04000;
   ADA_O_RSYNC : constant Int := 04010000;
   ADA_O_SYNC : constant Int := 04010000;
   ADA_O_ACCMODE : constant Int := 0003;
   ADA_O_RDONLY : constant Int := 00;
   ADA_O_RDWR : constant Int := 02;
   ADA_O_WRONLY : constant Int := 01;
   ADA_AT_FDCWD : constant Int := -100;
--   ADA_AT_EACCESS : constant Int := 0x200;
--   ADA_AT_SYMLINK_NOFOLLOW : constant Int := 0x100;
end Const_H;
