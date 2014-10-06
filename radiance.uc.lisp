
{EQL
 :SQLITE {EQL
          :DEFAULT :DEV,
          :CONNECTIONS {EQL
                        :MEMORY "-:memory:",
                        :DEV "d/dev.sqlite.db"}},
 :INTERFACES {EQL
              :CACHE "-r-simple-cache",
              :LOGGER "-i-verbose",
              :USER "-r-simple-users",
              :DATA-MODEL "-r-simple-model",
              :PROFILE "-r-simple-profile",
              :DATABASE "-i-sqlite",
              :ADMIN "-r-simple-admin",
              :AUTH "-r-simple-auth",
              :SESSION "-r-simple-sessions",
              :SERVER "-i-hunchentoot",
              :RATE "-r-simple-rate" },
 :POSTMODERN {EQL
              :DEFAULT :LOCALHOST,
              :CONNECTIONS {EQL
                            :TUNNEL {EQL
                                     :HOST "-127.0.0.1",
                                     :USER "-radiance",
                                     :DATABASE "-radiance",
                                     :PASS "-q#vRsABnFrJ417$!",
                                     :PORT 5432},
                            :LOCALHOST {EQL
                                        :HOST "-localhost",
                                        :USER "-radiance",
                                        :DATABASE "-radiance",
                                        :PASS "-q#vRsABnFrJ417$!",
                                        :PORT 5432}}},
 :PLASTER {EQL
           :CAPTCHA T,
           :ANON T},
 :SERVER {EQL
          :DOMAINS ("-localhost" "-linuz.com" "-radiance.test"),
          :INSTANCES ({EQL
                       :PORT 8080}
                      {EQL
                       :PORT 4545})}} 
