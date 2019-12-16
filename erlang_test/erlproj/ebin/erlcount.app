{application, erlcount,
 [{vsn, "1.0.0"},
  {modules, [erlcount, erlcount_sup, erlcount_lib,
             erlcount_dispatch, erlcount_counter]},
  {applications, [ppool]},
  {registered, [erlcount]},
  {mod, {erlcount, []}},
  {env,
   [{directory, "."},
    {regex, ["if\\s.+->", "case\\s.+\\sof"]},
    {max_files, 10}]}
 ]}.

 
%%http://learnyousomeerlang.com/building-otp-applications
%%http://erlang.org/doc/man/app.html
%%http://learnyousomeerlang.com/the-count-of-applications