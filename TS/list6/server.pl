  use HTTP::Daemon;
  use HTTP::Status;  
  #use IO::File;

  my $d = HTTP::Daemon->new(
           LocalAddr => 'localhost',
           LocalPort => 1234,
       )|| die;
  
  print "Please contact me at: <URL:", $d->url, ">\n";


  while (my $c = $d->accept) {
      while (my $r = $c->get_request) {
          if ($r->method eq 'GET') {
              
              $file_s= "./main.html";    # index.html - jakis istniejacy plik
              $c->send_file_response($file_s);

          }
          else {
              $c->send_error(RC_FORBIDDEN)
          }

      }
      $c->close;
      undef($c);
  }
