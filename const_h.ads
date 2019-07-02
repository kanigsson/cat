with Interfaces.C; use Interfaces.C;
package Const_H with
  SPARK_Mode
is
   ADA_F_DUPFD : constant int := 0;
   ADA_F_DUPFD_CLOEXEC : constant int := 1030;
   ADA_F_GETFD : constant int := 1;
   ADA_F_SETFD : constant int := 2;
   ADA_F_GETFL : constant int := 3;
   ADA_F_SETFL : constant int := 4;
   ADA_F_GETLK : constant int := 5;
   ADA_F_SETLK : constant int := 6;
   ADA_F_SETLKW : constant int := 7;
   ADA_F_GETOWN : constant int := 9;
   ADA_F_SETOWN : constant int := 8;
   ADA_FD_CLOEXEC : constant int := 1;
   ADA_F_RDLCK : constant int := 0;
   ADA_F_UNLCK : constant int := 2;
   ADA_F_WRLCK : constant int := 1;
   ADA_O_CLOEXEC : constant int := 02000000;
   ADA_O_CREAT : constant int := 0100;
   ADA_O_DIRECTORY : constant int := 0200000;
   ADA_O_EXCL : constant int := 0200;
   ADA_O_NOCTTY : constant int := 0400;
   ADA_O_NOFOLLOW : constant int := 0400000;
   ADA_O_TRUNC : constant int := 01000;
   ADA_O_APPEND : constant int := 02000;
   ADA_O_DSYNC : constant int := 010000;
   ADA_O_NONBLOCK : constant int := 04000;
   ADA_O_RSYNC : constant int := 04010000;
   ADA_O_SYNC : constant int := 04010000;
   ADA_O_ACCMODE : constant int := 0003;
   ADA_O_RDONLY : constant int := 00;
   ADA_O_RDWR : constant int := 02;
   ADA_O_WRONLY : constant int := 01;
   ADA_AT_FDCWD : constant int := -100;
--     ADA_AT_EACCESS : constant Int := 0x200;
--     ADA_AT_SYMLINK_NOFOLLOW : constant Int := 0x100;
end Const_H;
