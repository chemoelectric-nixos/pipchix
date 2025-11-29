/*

 Copyright © 2025 Barry Schwartz

 This file is part of Pipchix.

 Permission is hereby granted, free of charge, to any person obtaining
 a copy of Pipchix and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>

static struct timespec
modification_timespec (char *file)
{
  struct stat buf;
  int retval = stat (file, &buf);
  if (retval != 0)
    {
      fprintf (stderr, "failure to stat '%s'\n", file);
      exit (99);
    }
  return buf.st_mtim;
}

static int
compare_timespecs (struct timespec ts1, struct timespec ts2)
{
  int retval;
  if (ts1.tv_sec < ts2.tv_sec)
    retval = -1;
  else if (ts1.tv_sec > ts2.tv_sec)
    retval = 1;
  else if (ts1.tv_nsec < ts2.tv_nsec)
    retval = -1;
  else if (ts1.tv_nsec > ts2.tv_nsec)
    retval = 1;
  else
    retval = 0;
  return retval;
}

static int
compare_each_pair (char *file1, int num_files, char **file)
{
  // Return immediately, with return value zero, if file1 is older
  // than any of the other files.

  struct timespec ts_file1 = modification_timespec (file1);

  int retval = 1;
  int i = 0;
  while (retval == 1 && i != num_files)
    {
      struct timespec ts_filei = modification_timespec (file[i]);
      if (compare_timespecs (ts_file1, ts_filei) < 0)
	retval = 0;
      else
	i += 1;
    }
  return retval;
}

int
main (int argc, char **argv)
{
  int exit_status = 0;

  if (argc < 3)
    {
      fprintf (stderr, "At least two arguments are required.\n");
      exit (99);
    }

  exit_status = compare_each_pair (argv[1], argc - 2, argv + 2);
  return exit_status;
}
