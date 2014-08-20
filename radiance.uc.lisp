
{EQL
 :INTERFACES {EQL
              :DATABASE "-i-postmodern",
              :DATA-MODEL "-r-simple-model",
              :LOGGER "-i-verbose",
              :SERVER "-i-hunchentoot",
              :SESSION "-r-simple-sessions",
              :USER "-r-simple-users",
              :AUTH "-r-simple-auth",
              :CACHE "-r-simple-cache",
              :PROFILE "-r-simple-profile"},
 :SERVER {EQL
               :INSTANCES ({EQL
                            :PORT 8080}
                           {EQL
                            :PORT 4545}),
               :DOMAINS ("-localhost" "-linuz.com")},
 :POSTMODERN {EQL
              :CONNECTIONS {EQL
                            :LOCALHOST {EQL
                                        :HOST "-localhost",
                                        :PORT 5432,
                                        :USER "-radiance",
                                        :PASS "-q#vRsABnFrJ417$!",
                                        :DATABASE "-radiance"},
                            :TUNNEL {EQL
                                     :HOST "-127.0.0.1",
                                     :PORT 5432,
                                     :USER "-radiance",
                                     :PASS "-q#vRsABnFrJ417$!",
                                     :DATABASE "-radiance"}},
              :DEFAULT :TUNNEL},
 :PLASTER {EQL
           :ANON T,
           :CAPTCHA T}} 
