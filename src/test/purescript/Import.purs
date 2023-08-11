module Test.Import where

import Control.Bind (discard, bind)
import Data.Array (length, head, filter)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import DataModel.Card (Card(..), CardValues(..), CardField(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Import (decodeImport)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import TestUtilities (makeTestableOnBrowser)

importSpec :: SpecT Aff Unit Identity Unit
importSpec  =
  describe "Import" do
    -- decodeResult <- liftEffect $ decodeImport joe_clipperzData

    let emptyImport = "Empty json"
    it emptyImport do
      res <- liftEffect $ decodeImport "[]"
      makeTestableOnBrowser emptyImport (res) shouldEqual (Right [])

    let cardsNumber = "Import the correct number of cards"
    it cardsNumber do
      decodeResult <- liftEffect $ decodeImport joe_clipperzData
      makeTestableOnBrowser cardsNumber (length <$> decodeResult) shouldEqual (Right 39)

    let importFirstCard = "Import correctly the first card"
    it importFirstCard do
      decodeResult <- liftEffect $ decodeImport joe_clipperzData
      let result = Card { timestamp: 0.0
                           , archived: false
                           , secrets: []
                           , content: CardValues { title: "Amazon.com"
                                                     , tags: ["shopping"]
                                                     , fields: [ CardField { name: "email", value: "joe@clipperz.com", locked: false, settings: Nothing }
                                                               , CardField { name: "password", value: "8gJcYP~bJh#PMfA[|eU", locked: true, settings: Nothing }
                                                               , CardField { name: "URL", value: "https://www.amazon.com", locked: false, settings: Nothing }
                                                               , CardField { name: "", value: "", locked: true, settings: Nothing }
                                                               ]
                                                     , notes: ""
                                                     }
                           }
      makeTestableOnBrowser importFirstCard (head <$> decodeResult) shouldEqual (Right $ Just result)
    
    let importArchivedCard = "Import archived card"
    it importArchivedCard do
      decodeResult <- liftEffect $ decodeImport joe_clipperzData
      let result = Card { timestamp: 0.0
                           , archived: true
                           , secrets: []
                           , content: CardValues { title: "AOL "
                                                     , tags: ["", "social"]
                                                     , fields: [ CardField { name: "URL", value: "http://www.aol.com", locked: false, settings: Nothing }
                                                               , CardField { name: "ID", value: "88440023", locked: false, settings: Nothing }
                                                               , CardField { name: "password", value: "I9EJpXaOzNoNATZB0NjUcUZYBa", locked: true, settings: Nothing }
                                                               ]
                                                     , notes: "Ah the good old times. :)"
                                                     }
                           }
      makeTestableOnBrowser importArchivedCard ((\a -> filter (\(Card { content: CardValues { title } }) -> title == "AOL ") a) <$> decodeResult) shouldEqual (Right $ [result])
      -- makeTestableOnBrowser importArchivedCard ((\a -> index a 3) <$> decodeResult) shouldEqual (Right $ Just result)
         
    where
      joe_clipperzData = """
        [
          {
            "label": "Amazon.com shopping",
            "data": {
              "directLogins": {
                "65f93ac9c0f0787dc5d149a44e574140df26cde715372a534189e41658c7f828": {
                  "favicon": "http://www.amazon.com/favicon.ico",
                  "label": "Amazon.com",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.amazon.com/ap/signin",
                      "method": "POST"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "appActionToken",
                        "value": "znS4W1cAKAsbZ6ueGl703j2B5xpncj3D"
                      },
                      {
                        "type": "hidden",
                        "name": "appAction",
                        "value": "SIGNIN"
                      },
                      {
                        "type": "hidden",
                        "name": "openid.pape.max_auth_age",
                        "value": "ape:MA=="
                      },
                      {
                        "type": "hidden",
                        "name": "openid.ns",
                        "value": "ape:aHR0cDovL3NwZWNzLm9wZW5pZC5uZXQvYXV0aC8yLjA="
                      },
                      {
                        "type": "hidden",
                        "name": "openid.ns.pape",
                        "value": "ape:aHR0cDovL3NwZWNzLm9wZW5pZC5uZXQvZXh0ZW5zaW9ucy9wYXBlLzEuMA=="
                      },
                      {
                        "type": "hidden",
                        "name": "prevRID",
                        "value": "ape:MDhZODNIN1A0S0I1M0dINERGTUU="
                      },
                      {
                        "type": "hidden",
                        "name": "pageId",
                        "value": "ape:dXNmbGV4"
                      },
                      {
                        "type": "hidden",
                        "name": "openid.identity",
                        "value": "ape:aHR0cDovL3NwZWNzLm9wZW5pZC5uZXQvYXV0aC8yLjAvaWRlbnRpZmllcl9zZWxlY3Q="
                      },
                      {
                        "type": "hidden",
                        "name": "openid.claimed_id",
                        "value": "ape:aHR0cDovL3NwZWNzLm9wZW5pZC5uZXQvYXV0aC8yLjAvaWRlbnRpZmllcl9zZWxlY3Q="
                      },
                      {
                        "type": "hidden",
                        "name": "openid.mode",
                        "value": "ape:Y2hlY2tpZF9zZXR1cA=="
                      },
                      {
                        "type": "hidden",
                        "name": "openid.assoc_handle",
                        "value": "ape:dXNmbGV4"
                      },
                      {
                        "type": "hidden",
                        "name": "openid.return_to",
                        "value": "ape:aHR0cHM6Ly93d3cuYW1hem9uLmNvbS9ncC95b3Vyc3RvcmUvaG9tZT9pZT1VVEY4JnJlZl89bmF2X3NpZ25pbg=="
                      },
                      {
                        "type": "email",
                        "name": "email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "metadata1",
                        "value": "AaRAdCjuYJnaEp3D2riGe04KbBp8xHMiYfDPCoZRzra8LRJDmIxiVyboxUbd6KtPJzmPN7XDKpRwCoqA+YuDn4nxjf2CgUMjK6+9oLT0j2JDX8vE/KWqwSbIlzilxpw5dGh/i7pYa5Z+H0mB23c43T92HfGffSywjrRWFM0yqD1+0vM5H+CBh20upJgckoynOVIEsTHAnwOZDETvocP1GpFieh+IfEF1z/SDfRNIuLexxmj7+ffsVTX3xvt45764LXhsCx6zIQ3EHQFswCxjw7NNCZqHjOC7bRHAZwsAKF6PjcikHM16dDqOTNKZEVJSt1O67mqK1k1aJqdFnkFsMmUdqvA1fn8syqPwNQmrjlGx6DThj6LHhJHG5iiFsE0PcHpYVtzKPxavmt8VEDV6MlgJgNZZ5bysWc/ajHFq/nj6QLMA67mh9jj0k8i1P3h6H8mo5kNOwWFYqIxwEBY0iBo5wNLXpGLTGQbEl455F0XexOajj230O6tMeMpEDXJoUstwwkzOMAgRYIpsswzU76WC7DHCI0FMRgcL0w1P6NV+kx7NtdykTX1QPYGvHhc7+z2MloBVweWpQ499NCli0+9SIHgLxBKNq9dmMQ=="
                      },
                      {
                        "name": "create",
                        "type": "radio",
                        "options": [
                          {
                            "value": "1",
                            "checked": false
                          },
                          {
                            "value": "0",
                            "checked": true
                          }
                        ]
                      }
                    ]
                  },
                  "formValues": {
                    "create": null
                  },
                  "bindingData": {
                    "password": "305679e5119e45cf50189017e3d91627915bea60eed18e0f544119d877d022bf"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "852faeb91fb9bd218b2e647d10641cf4d592ea30210f0caebe8ec05a44211ed0": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "b5358e7e9c5728d5776797742dc1b20670eef991de9f4434131ba7b181c086dd": {
                  "label": "password",
                  "value": "8gJcYP~bJh#PMfA[|eU",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "ad924e1340ffc815bd3956b7d501fdd4dfd619952dcfb7ac900717ef5cdd9fb7": {
                  "label": "URL",
                  "value": "https://www.amazon.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "1fc1dc1f6659b0d29d19570c5ca91d3ea229c6ab03640bb17b60a8572ea6ea51": {
                  "label": "",
                  "value": "",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "American Airlines travel",
            "data": {
              "directLogins": {
                "632f40721cf56606f5c8227020e4a574585feb522cd410872e741ccde348adf3": {
                  "favicon": "http://www.americanairlines.it/favicon.ico",
                  "label": "American Airlines",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.americanairlines.it/login/loginSubmit.do",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "requestContextId",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "previousPage",
                        "value": "/myAccount/myAccountAccess.do"
                      },
                      {
                        "type": "hidden",
                        "name": "CurrentProcessId",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "bookingPathStateId",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "marketId",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "discountCode",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "uri",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "seatPayment",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "vpayment",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "loginId",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "rememberMe",
                        "value": "on"
                      },
                      {
                        "type": "submit",
                        "name": "_button_login",
                        "value": "Login"
                      }
                    ]
                  },
                  "formValues": {
                    "rememberMe": false
                  },
                  "bindingData": {
                    "loginId": "e2d874c9a326c31605bdc0ec7ef47ecd258538b5dfdab49863aff5d148471c2d",
                    "password": "4a311bce428eeff7912541cf6c2d55e654e10e9405c1d146c06489b4609b6296"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "e2d874c9a326c31605bdc0ec7ef47ecd258538b5dfdab49863aff5d148471c2d": {
                  "label": "AAdvantage N.",
                  "value": "M3215J2-1",
                  "actionType": "NONE",
                  "hidden": false
                },
                "4a311bce428eeff7912541cf6c2d55e654e10e9405c1d146c06489b4609b6296": {
                  "label": "Password",
                  "value": "n19GeZPt2Wv6",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "31d750b1944b65454a47ab10ad8b04ce99464c68b5f85bb015817ae7433b3940": {
                  "label": "Web site",
                  "value": "https://www.aa.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "291de79bc741d0cf8471fd79dceff54a6f073588988755df284439247986e24e": {
                  "label": "Call center",
                  "value": "1-800-222-2377 ",
                  "actionType": "NONE",
                  "hidden": false
                },
                "3d36b42659e9f320ec714d2f889b3dafa0a96580627c84b7595392e5ebebd72d": {
                  "label": "Expire date",
                  "value": "24-11-2018",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "American Express finance cards",
            "data": {
              "directLogins": {
                "69f76440f9c5fa0c66bf4d0b2742c8211f30dcf562d3aaac28e56216d53c2f27": {
                  "favicon": "http://www.americanexpress.com/favicon.ico",
                  "label": "American Express",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.americanexpress.com/",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "DestPage",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "Face",
                        "value": "en_US"
                      },
                      {
                        "type": "hidden",
                        "name": "USERID",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "PWD",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "CHECKBOXSTATUS",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "devicePrint",
                        "value": "version%3D1%26pm%5Ffpua%3Dmozilla%2F5%2E0%20%28macintosh%3B%20intel%20mac%20os%20x%2010%2E10%3B%20rv%3A36%2E0%29%20gecko%2F20100101%20firefox%2F36%2E0%7C5%2E0%20%28Macintosh%29%7CMacIntel%26pm%5Ffpsc%3D24%7C1920%7C1080%7C1057%26pm%5Ffpsw%3D%26pm%5Ffptz%3D1%26pm%5Ffpln%3Dlang%3Den%2DUS%7Csyslang%3D%7Cuserlang%3D%26pm%5Ffpjv%3D1%26pm%5Ffpco%3D1"
                      },
                      {
                        "type": "hidden",
                        "name": "brandname",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "TARGET",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "request_type",
                        "value": "IntlLogLogonHandler"
                      },
                      {
                        "type": "hidden",
                        "name": "Logon",
                        "value": "Continue..."
                      },
                      {
                        "type": "hidden",
                        "name": "act",
                        "value": ""
                      },
                      {
                        "type": "select",
                        "name": "cardsmanage",
                        "options": [
                          {
                            "selected": true,
                            "label": "Cards - My Account",
                            "value": "cards"
                          },
                          {
                            "selected": false,
                            "label": "Membership Rewards",
                            "value": "membershiprewards"
                          },
                          {
                            "selected": false,
                            "label": "Merchant Account",
                            "value": "merchantact"
                          },
                          {
                            "selected": false,
                            "label": "American Express @ Work",
                            "value": "atwork"
                          }
                        ]
                      },
                      {
                        "type": "text",
                        "name": "UserID",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "Password",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "REMEMBERME",
                        "value": "on"
                      }
                    ]
                  },
                  "formValues": {
                    "cardsmanage": "null",
                    "REMEMBERME": false
                  },
                  "bindingData": {
                    "UserID": "963d8bc657d51c4ad792447d0d7c7ed1982c6a2f24b7aa94d1c9f1099fd2d9c1",
                    "Password": "7934bf28a72b7abc7bba171dba44ca0c260019a07a74632604c5e38ded758b59"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "565a78505fb0c95334c371ae4f1426837568966ca3a559279d4362903f6d8951": {
                  "label": "Type",
                  "value": "American Express",
                  "actionType": "NONE",
                  "hidden": false
                },
                "d3acacff619bda5f6cddcf3b7e1ae18342a0a9937e926d15d86529c59f048c6e": {
                  "label": "Number",
                  "value": "4547 3944 2340 3445",
                  "actionType": "NONE",
                  "hidden": false
                },
                "cf7e8a55bb19e36b9de398de6bcbdecd67a6546e95268e7020991b568605ca85": {
                  "label": "Cardholder",
                  "value": "Joe Clipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "249ebe819111515a04c3f946ff6425debf7c75de215e6b5e223e0814958471b7": {
                  "label": "Expiry date",
                  "value": "06 / 18",
                  "actionType": "NONE",
                  "hidden": false
                },
                "2da8a47cc1eac674b40485a0a3568ea2ca135531db4c25cf53368f344bad85bf": {
                  "label": "CVV2",
                  "value": "466",
                  "actionType": "NONE",
                  "hidden": false
                },
                "5c219ca50af949f7c9bc74ce2510e360ebee13ea26a22b05d5e6c607fd3a7e12": {
                  "label": "PIN",
                  "value": "30554",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "a588e6f3113ddf7efd5efdbd534036d8cffec715e1a05c8c71385b23acb5087b": {
                  "label": "Card website",
                  "value": "https://www.americanexpress.com/",
                  "actionType": "URL",
                  "hidden": false
                },
                "963d8bc657d51c4ad792447d0d7c7ed1982c6a2f24b7aa94d1c9f1099fd2d9c1": {
                  "label": "Username",
                  "value": "jclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "7934bf28a72b7abc7bba171dba44ca0c260019a07a74632604c5e38ded758b59": {
                  "label": "Password",
                  "value": "mIOBiGyGtkNjebRjk5zdKV",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "AOL   social ARCH",
            "data": {
              "directLogins": {
              },
              "notes": "Ah the good old times. :)"
            },
            "currentVersion": {
              "fields": {
                "95251ed8f11ec4a23bf622a492a9f17dd9e97fe1186c7670601ea3edf07dfa09": {
                  "label": "URL",
                  "value": "http://www.aol.com",
                  "actionType": "URL",
                  "hidden": null
                },
                "822cf14086e3a52fe5ecb721d4f8a9f9f22d930f1cc820ee680cdc52926e56a0": {
                  "label": "ID",
                  "value": "88440023",
                  "actionType": "NONE",
                  "hidden": null
                },
                "dc278042d9341c0567e145f3f007d613a92af4e94b3b47a884b5a51743a030c3": {
                  "label": "password",
                  "value": "I9EJpXaOzNoNATZB0NjUcUZYBa",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "AT&T Mobile family",
            "data": {
              "directLogins": {
                "a8818157b59693c573fdff99b3932ee95c8b228670ecd8a2944479edbb91249f": {
                  "favicon": "http://www.att.com/favicon.ico",
                  "label": "myAT&T",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.att.com/olam/passthroughAction.myworld?actionType=Manage#",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "userid",
                        "value": ""
                      },
                      {
                        "type": "Password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "source",
                        "value": "MYATT"
                      },
                      {
                        "type": "hidden",
                        "name": "flow_ind",
                        "value": "LGN"
                      },
                      {
                        "type": "hidden",
                        "name": "vhname",
                        "value": "www.att.com"
                      },
                      {
                        "type": "hidden",
                        "name": "urlParameters",
                        "value": "actionType=Manage&reportActionEvent=A_LGN_LOGIN_SUB&loginSource=olam"
                      },
                      {
                        "type": "hidden",
                        "name": "isSlidLogin",
                        "value": "true"
                      },
                      {
                        "type": "hidden",
                        "name": "remember_me",
                        "value": "N"
                      },
                      {
                        "type": "hidden",
                        "name": "cancelURL",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "rootPath",
                        "value": "/olam/English"
                      },
                      {
                        "type": "hidden",
                        "name": "persist",
                        "value": "y"
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "userid": "3bd6c97a69eb299852408b08bb6d7f52ed40dbcefb4c040cbfe2522781b78adf",
                    "password": "48989534fec1e4c8f81961868ae5dd02ad1ee234a3b6e9b925d23042ef7f203d"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "3bd6c97a69eb299852408b08bb6d7f52ed40dbcefb4c040cbfe2522781b78adf": {
                  "label": "userid",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "48989534fec1e4c8f81961868ae5dd02ad1ee234a3b6e9b925d23042ef7f203d": {
                  "label": "password",
                  "value": "fKcIYnL2jCvSUyJVh1UgOz",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "29ac41fa460bffddec0ce6b930ef6c0934982d68ca5001c4b2e5068594fa8f8e": {
                  "label": "URL",
                  "value": "https://www.att.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "e32c1105b0d1722536bc61dcb7b287c1b7606f61dcf53b8cbed686ec7e434a6e": {
                  "label": "cell phone n.",
                  "value": "+1 212 8834 4443",
                  "actionType": "NONE",
                  "hidden": false
                },
                "c8d6cde4a6eb15053b48de7d4d9fea5b0f8a9600d58c7708e019aedfbd627cd1": {
                  "label": "SIM n.",
                  "value": "84856-0040-77233944",
                  "actionType": "NONE",
                  "hidden": false
                },
                "4ead84bdc11a6463a85caf770acd59aa18f4517fb4d136bad3496de21c7f1c4c": {
                  "label": "PUK",
                  "value": "37-847642",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Bike lock family security",
            "data": {
              "directLogins": {
              },
              "notes": "Extra note."
            },
            "currentVersion": {
              "fields": {
                "64134236f63dcbea8eb3af3081b67d5510b645efd9681ba81ac60ff08b4da0cf": {
                  "label": "Combination",
                  "value": "7499",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Bloglines   reading ARCH",
            "data": {
              "directLogins": {
              },
              "notes": "The best RSS reader ever existed!"
            },
            "currentVersion": {
              "fields": {
                "e8be627dd18c5d95572b1975aa9908177878f93f9ea0ad7751b1ebfbd9f259aa": {
                  "label": "URL",
                  "value": "http://www.bloglines.com",
                  "actionType": "URL",
                  "hidden": null
                },
                "04b42c0207a0670a118aa9e37fc81b50ab7abbca14ad5488c382be448a689245": {
                  "label": "email",
                  "value": "joe.clipperz@gmail.com",
                  "actionType": "EMAIL",
                  "hidden": null
                },
                "ce7d59afcccf54c14fb9ba04f67554e80b1c34620bb43e774e284e07a1e423c9": {
                  "label": "username",
                  "value": "jclipperz",
                  "actionType": "NONE",
                  "hidden": null
                },
                "5011708b77f8de66ddc9e0fe687013e46bc8432d956f4ff3df6567dad189e83d": {
                  "label": "password",
                  "value": "28AR1QDnhodMwo99SwE7HC3gCt",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Coinbase bitcoin finance",
            "data": {
              "directLogins": {
                "5859b5eb97e6a342de219e49bce04c29f9c6aeea77658a699ef27ec2f943e18d": {
                  "favicon": "http://www.coinbase.com/favicon.ico",
                  "label": "Coinbase",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.coinbase.com/sessions",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "utf8",
                        "value": "✓"
                      },
                      {
                        "type": "hidden",
                        "name": "authenticity_token",
                        "value": "iRLkpvqlgXKWnxBfX2ser4HUa7nFhxAPeJT31xuRvNg="
                      },
                      {
                        "type": "email",
                        "name": "email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "submit",
                        "name": "commit",
                        "value": "Sign In"
                      },
                      {
                        "type": "checkbox",
                        "name": "stay_signed_in",
                        "value": "1"
                      }
                    ]
                  },
                  "formValues": {
                    "stay_signed_in": false
                  },
                  "bindingData": {
                    "password": "3e469cc3177822b601e9b8b11b8437c838e1a766418f64f1be681039336a9672"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "5727be068598a180e6592b29daf74f0522a25498cba240a092667a08b11c0fec": {
                  "label": "URL",
                  "value": "https://www.coinbase.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "256852dbd389b1987de6ce07b6da43207da8c7d79f824bbb2245b75a1d90e01f": {
                  "label": "Email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "3e469cc3177822b601e9b8b11b8437c838e1a766418f64f1be681039336a9672": {
                  "label": "Password",
                  "value": "JKicNe4UTxjb9jGLz9czGp",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "bd741893cd26cc6d89bd2d8c2c0bf074b16a96d56255d78efdb4911b38c5e93e": {
                  "label": "Wallet address",
                  "value": "1J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy",
                  "actionType": "NONE",
                  "hidden": false
                },
                "02796906e72152281fdd27f9dc904d15efc1adc95a2f665d227df7ca47bb2991": {
                  "label": "test",
                  "value": "",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "del.icio.us reading",
            "data": {
              "directLogins": {
                "f4cc4604c1db808cfab53354260b01ae1f2826c621ac4a3cc499f132ac30f02b": {
                  "favicon": "http://del.icio.us/favicon.ico",
                  "label": "Delicious",
                  "bookmarkletVersion": "0.3.0",
                  "formData": {
                    "attributes": {
                      "action": "http://del.icio.us/login",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "username",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "username": "3c84422f216ad6bdda05afbe9d97e1f43a1c7f0b79e3143ec3650323658e03f1",
                    "password": "d88851a2190fc7def62ca8488b9e03c0462104e712d553fa73108ad0d7623ce4"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "3c84422f216ad6bdda05afbe9d97e1f43a1c7f0b79e3143ec3650323658e03f1": {
                  "label": "username",
                  "value": "joe_clipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "d88851a2190fc7def62ca8488b9e03c0462104e712d553fa73108ad0d7623ce4": {
                  "label": "password",
                  "value": "deQuEsSoc25",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "078803027ddeb24abb3267522f5be42c74629299d6f1a2060049e75ee9346d06": {
                  "label": "URL",
                  "value": "http://del.icio.us",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Django Unchained Reloaded scripts movies",
            "data": {
              "directLogins": {
              },
              "notes": "Two years after the Civil War, Django, finds himself accompanying the Swedish physician Dr. Arne Holm on a mission to cure jungle fever. "
            },
            "currentVersion": {
              "fields": {
                "ae8bce44032f21d760c5510b2415754fc2dc4bd457bc26aba6fa889bf86aea1d": {
                  "label": "Co-authors",
                  "value": "Quentin Tarantino Woody Allen",
                  "actionType": "NONE",
                  "hidden": false
                },
                "a9b7be4dc218ac7673a3e87e453f7b6cb5b9c9fa69a1cb9785c383c928a6fb80": {
                  "label": "Revision",
                  "value": "N. 23/2015",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Driver license family",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "7c69682955a674e62de0a3c3f83abc206b3e8ed55668ae7e5dc31dbf1d4838d4": {
                  "label": "License n.",
                  "value": "084 210 74B",
                  "actionType": "NONE",
                  "hidden": null
                },
                "0dfe999a294a3648936d81d595fd0e2b9d5e972271116775e989cb9641d60981": {
                  "label": "Expiry date",
                  "value": "08/31/2020",
                  "actionType": "NONE",
                  "hidden": null
                },
                "7875893c8cfb78a6818dcd8ef3df2ebf1906aafe0979e237c970693cc3a08f38": {
                  "label": "Issue date",
                  "value": "09/01/2010",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "easyJet travel",
            "data": {
              "directLogins": {
                "9613ee2be520c7de0af9f67cc5048ebb649fb4a472717bbdb6822b299a4c7f55": {
                  "favicon": "http://www.easyjet.com/favicon.ico",
                  "label": "easyJet",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.easyjet.com/mylogin/en-GB/LogOnAsMember?ReturnUrl=%2Fmylogin%2Fen-GB%3Fwa%3Dwsignin1.0%26wtrealm%3Durn%253a%252fen%252fsecure%252fFederation.mvc%252fFederatedSignIn%26wctx%3Dhttp%253a%252f%252fwww.easyjet.com%252fen%252fMyEasyJet%252fViewBooking%26wct%3D2015-03-04T09%253a20%253a55.0900354Z",
                      "method": "POST"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "emailaddress",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "memberbookingreference",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "passengerbookingreference",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "firstname",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "lastname",
                        "value": ""
                      },
                      {
                        "name": "login-made-booking-radio",
                        "type": "radio",
                        "options": [
                          {
                            "value": "2",
                            "checked": true
                          },
                          {
                            "value": "1",
                            "checked": false
                          }
                        ]
                      }
                    ]
                  },
                  "formValues": {
                    "login-made-booking-radio": null
                  },
                  "bindingData": {
                    "emailaddress": "1e385cfd3b59021cdaeb13efdbcbb09302478ab3b29a081b4446fd814cf33cf6",
                    "password": "ead89650dec54f6c18884c48d7381c3befd1da757baca25c2c2b4de6cfc501ce",
                    "memberbookingreference": "null",
                    "passengerbookingreference": "null",
                    "firstname": "null",
                    "lastname": "null"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "1e385cfd3b59021cdaeb13efdbcbb09302478ab3b29a081b4446fd814cf33cf6": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "ead89650dec54f6c18884c48d7381c3befd1da757baca25c2c2b4de6cfc501ce": {
                  "label": "password",
                  "value": "4QdGK6Zugy8APt1QRsSuTS",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "9de1aedfef83f4d18a1da657509745c9dade613aa481fcb6ca6380245fbf9472": {
                  "label": "URL",
                  "value": "https://www.easyjet.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Etsy shopping",
            "data": {
              "directLogins": {
                "33a73db6f9626dd260e9d33ca2e6000518b6eea8b7a3e5e55fc4fd4add4d43ab": {
                  "favicon": "http://www.etsy.com/favicon.ico",
                  "label": "Etsy",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.etsy.com/signin?from_page=https%3A%2F%2Fwww.etsy.com%2F",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "d9c3908a",
                        "value": "54f72f1e"
                      },
                      {
                        "type": "hidden",
                        "name": "external_account_id",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "external_avatar",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "external_username",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "external_account_type_name",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "external_name",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "google_auth_code",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "connect_facebook",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "connect_google",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "username",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "persistent",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "_nnc",
                        "value": "3:1425485599:dca8gWgi5oKkqUTCrxrNdKMGNe5L:d487a8f76cff488374f5497f770d6c4bea21d4ad7fbae2d4c021e890f40baa0a"
                      },
                      {
                        "type": "hidden",
                        "name": "from_page",
                        "value": "https://www.etsy.com/"
                      },
                      {
                        "type": "hidden",
                        "name": "from_action",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "from_overlay",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "_nnc",
                        "value": "3:1425485599:pCFJVcunGrjPwq1tX5V-0-MByHM4:c7f636b44229b07ed4a82d1e7c7a5eb1d7e57f3b0d8738cf30fa8987a803ff43"
                      },
                      {
                        "type": "hidden",
                        "name": "etsy_device_id",
                        "value": "{\"a\":\"Mac OSX (Version Unknown)\",\"b\":\"MacIntel\",\"c\":1,\"d\":\"24|1920|1080|1916|1057\",\"e\":\"141f9176e7938ccd4f8299a078d53491\",\"f\":\"Firefox 36\",\"g\":\"f5f540f9f0ac759b49cb7799ebd7eb8c\",\"h\":\"BRW=en-US|SYS=|USR=\",\"i\":\"Not Installed\",\"j\":\"Not Installed\",\"k\":1,\"l\":1,\"m\":\"a06f55412d3f05a8bd9285b1d0153712\",\"t\":177}"
                      }
                    ]
                  },
                  "formValues": {
                    "persistent": false
                  },
                  "bindingData": {
                    "username": "0310d21c476798ba4bd359e0b9c1d5543ec2c9be5fe60a283577e6abb0b27bb0",
                    "password": "1b7d5d02eb70fb5a8664187d85963c289e2da0ed0e717308318d8ba6b55cb5ce"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "0310d21c476798ba4bd359e0b9c1d5543ec2c9be5fe60a283577e6abb0b27bb0": {
                  "label": "username",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "1b7d5d02eb70fb5a8664187d85963c289e2da0ed0e717308318d8ba6b55cb5ce": {
                  "label": "password",
                  "value": "dljhd#98hdl449DLKDF",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "d5e61148320c9d7c4d6f57227c1f0ab1f6fc6ce10dd0bc9765373192d25e6ca1": {
                  "label": "URL",
                  "value": "https://www.etsy.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Expedia.com travel",
            "data": {
              "directLogins": {
                "f22dc41ffabef4b3bc8f7af804fec975bd50718098322a673cbe4aaff9464ae1": {
                  "favicon": "http://www.expedia.com/favicon.ico",
                  "label": "Expedia.com",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.expedia.com/pub/agent.dll",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "usri",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "upwd",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "usav",
                        "value": "1"
                      },
                      {
                        "type": "checkbox",
                        "name": "tccb",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "ussl",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "qscr",
                        "value": "logi"
                      },
                      {
                        "type": "hidden",
                        "name": "subl",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "lmde",
                        "value": "25"
                      },
                      {
                        "type": "hidden",
                        "name": "uact",
                        "value": "3"
                      },
                      {
                        "type": "hidden",
                        "name": "uurl",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "fram",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "wdth",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "hght",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "gpid",
                        "value": "C098DF09C249"
                      },
                      {
                        "type": "hidden",
                        "name": "slnk",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "flag",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "tmpu",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                    "usav": false,
                    "tccb": false
                  },
                  "bindingData": {
                    "usri": "6ab4729f72fe1c4078c8cc7d21aaa23777664485dec2a744a67c9b0644e068bf",
                    "upwd": "5f3c8229786aadd6c2ac7f94cd8f78396c01b58a1fce5bee0585215f79c03025"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "6ab4729f72fe1c4078c8cc7d21aaa23777664485dec2a744a67c9b0644e068bf": {
                  "label": "Username",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "5f3c8229786aadd6c2ac7f94cd8f78396c01b58a1fce5bee0585215f79c03025": {
                  "label": "Password",
                  "value": "5GXNXhGuJuPO",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "15b24ff856409fe3cbde8c5bfa9d05456eed8a859cf0d41914b56ce8f9c1d7cc": {
                  "label": "URL",
                  "value": "https://www.expedia.com",
                  "actionType": "URL",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Flickr family entertainment ARCH",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "745ca3d4e268dcb6dc704600ddfec3630ffc2d894687e9e94e9306e5674b7a32": {
                  "label": "URL",
                  "value": "https://www.flickr.com",
                  "actionType": "URL",
                  "hidden": null
                },
                "590d791339526afd685e607be66cfcf6317bc2e3ad039ea4d5ce799af29709df": {
                  "label": "email",
                  "value": "joe.clipperz.com",
                  "actionType": "NONE",
                  "hidden": null
                },
                "de21c25de1db2504b4eadbc2e6a6f42c120a98dbef98acec56d609b55b012305": {
                  "label": "password",
                  "value": "UTldyaYyrODs1iURvF6tDMdIIi",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Home burglar alarm family",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "337f52900ea9e947f8d22e0049309f4e541259045892cefd4f3c202f86d9a8b3": {
                  "label": "Activation code",
                  "value": "723439",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "b2fd9382f06b9d513b02c8100cd5771fa08c9eb59b3b57f28638a26a378e1600": {
                  "label": "Reset code",
                  "value": "9462946745",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "b494389dcd4e49aa83b44b311f6e80707deba9aeb3d3c9bd2c098fa3845d6f1a": {
                  "label": "Tech support",
                  "value": "+1 800 6458 343",
                  "actionType": "NONE",
                  "hidden": false
                },
                "6b03fd277a6830871c54f2c9199bb1174d7001c0986bae645dc3bf3a182157c1": {
                  "label": "Web access",
                  "value": "http://www.burglarmonitor.net",
                  "actionType": "URL",
                  "hidden": false
                },
                "5b74de9a59c70265db0d74cfd84e9686a1b3645d192660a78532bd0fd95288c0": {
                  "label": "Web username",
                  "value": "joe.smith",
                  "actionType": "NONE",
                  "hidden": false
                },
                "1e715356dfaa2bb9ec91b64afc97309b5396a1be398566df34b6663ab530c8dd": {
                  "label": "Web password",
                  "value": "763gs92sd90",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "HP Office Jet Pro 8600 hardware",
            "data": {
              "directLogins": {
                "c50495ba204a69758ef50a95851c7481845e4f9af342341b5bcd8023bc00226e": {
                  "favicon": "http://h30495.www3.hp.com/favicon.ico",
                  "label": "HP ePrintCenter",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://h30495.www3.hp.com/user_login",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "csrfmiddlewaretoken",
                        "value": "e42e0e419dfcdba3686eb1c5b8281629"
                      },
                      {
                        "type": "hidden",
                        "name": "csrfmiddlewaretoken",
                        "value": "e42e0e419dfcdba3686eb1c5b8281629"
                      },
                      {
                        "type": "hidden",
                        "name": "next",
                        "value": "/"
                      },
                      {
                        "type": "hidden",
                        "name": "countrylang",
                        "value": "US-en"
                      },
                      {
                        "type": "text",
                        "name": "email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "email": "null",
                    "password": "null"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "8b1f10f6b4fad12958ac6d89952bfd25fd770f4e9c56e78ee1ee710059c83811": {
                  "label": "Model",
                  "value": "HP Officejet Pro 8600 - N911a",
                  "actionType": "NONE",
                  "hidden": false
                },
                "ca8eee3fba077d9d978ba1271793438f5da64817d438490bf54194193f027928": {
                  "label": "Serial n.",
                  "value": "CN2CKBXJFZ",
                  "actionType": "NONE",
                  "hidden": false
                },
                "b1abbe11eedad0aaccdfa578856b7bbe8a0f17bf41c5ad78863d531ccaf44c45": {
                  "label": "Printer Email",
                  "value": "HP-parini29b@hpeprint.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "1cce249a68de95fc162245086402c6ddff8bd939611c8ae124182b5e79245023": {
                  "label": "URL",
                  "value": "https://www.hpeprint.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "c70d62ca52a5437e1c78137960b95985cc05f5c99b95be37127cb3fd81bb24aa": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "df77c24ed09a1569d337ad32ecbc417b9bcb5f2a9db1bde13aa1bff987464f11": {
                  "label": "password",
                  "value": "%\\:1kUmu#ev&/lD5T,!",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Last.fm entertainment",
            "data": {
              "directLogins": {
                "aad35cd020e3d78ba03681682096d1ab0303073c971c0282d0bf31d56843fd9f": {
                  "favicon": "http://www.last.fm/favicon.ico",
                  "label": "Last.fm",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.last.fm/login",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "refererKey",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "backto",
                        "value": "http://www.last.fm/home?join=1"
                      },
                      {
                        "type": "text",
                        "name": "username",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "submit",
                        "name": "login",
                        "value": "Come on in"
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "username": "d1bbc9da21579e819a20e4889e4075fb634785422cc5f895a7c226ad7e68e755",
                    "password": "628713f7b4028e9695e682db7b3045b013a6c9a8365f6c204d58d4c4a9402778"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "57c13d791eddc62104bf2ecd36a344453c32c6239528949ead63f073de78fa82": {
                  "label": "URL",
                  "value": "http://www.last.fm",
                  "actionType": "URL",
                  "hidden": false
                },
                "1a4b423f9aa5f718673cdb36dace1a93e676ad14e93f50b406678f87610c8433": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "d1bbc9da21579e819a20e4889e4075fb634785422cc5f895a7c226ad7e68e755": {
                  "label": "username",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "628713f7b4028e9695e682db7b3045b013a6c9a8365f6c204d58d4c4a9402778": {
                  "label": "password",
                  "value": "nbawohnmzxc34adc5",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "LinkedIn social work",
            "data": {
              "directLogins": {
                "61e87fdc4f1d9112e3b30c1f6812d095dcdb24f014c83319091eb6c9899ec348": {
                  "favicon": "http://www.linkedin.com/favicon.ico",
                  "label": "LinkedIn",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.linkedin.com/secure/login",
                      "method": "POST"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "session_key",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "session_password",
                        "value": ""
                      },
                      {
                        "type": "submit",
                        "name": "session_login",
                        "value": "Sign In"
                      },
                      {
                        "type": "hidden",
                        "name": "session_login",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "session_rikey",
                        "value": "invalid key"
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "session_key": "9ce82fb94a9918827b175fd9ba8857c8fafbbcdda9fa48ab46fa9bc199114999",
                    "session_password": "85a5972c15bb5da47dfd589bac822fa55ce3e6c3b560dccb9753cb42177b93a4"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "9ce82fb94a9918827b175fd9ba8857c8fafbbcdda9fa48ab46fa9bc199114999": {
                  "label": "Email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "85a5972c15bb5da47dfd589bac822fa55ce3e6c3b560dccb9753cb42177b93a4": {
                  "label": "Password",
                  "value": "6]67+*Y=zzDNx#/,|(j1?pTm",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "97e225385ff9904d26d488a780c0347e23f1e2f7056e3d9d072a0fbb39071d17": {
                  "label": "URL",
                  "value": "https://www.linkedin.com",
                  "actionType": "URL",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Lufthansa travel",
            "data": {
              "directLogins": {
                "e2231d46181622754c6326044448750caa9a9496a290a9661996e47cd5d51e3a": {
                  "favicon": "http://www.lufthansa.com/favicon.ico",
                  "label": "Lufthansa",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.lufthansa.com/online/portal/lh/cxml/04_SD9ePMtCP1I800I_KydQvyHFUBADPmuQy?l=en&cid=1000273&p=LH&s=IT",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "loginType",
                        "value": "socialLightboxLogin"
                      },
                      {
                        "type": "hidden",
                        "name": "portalId",
                        "value": "LH"
                      },
                      {
                        "type": "hidden",
                        "name": "siteId",
                        "value": "IT"
                      },
                      {
                        "type": "hidden",
                        "name": "current_nodeid",
                        "value": "992263173"
                      },
                      {
                        "type": "hidden",
                        "name": "current_taxonomy",
                        "value": "Homepage2011"
                      },
                      {
                        "type": "hidden",
                        "name": "countryId",
                        "value": "1000273"
                      },
                      {
                        "type": "hidden",
                        "name": "targetTaxonomy",
                        "value": "CPIT"
                      },
                      {
                        "type": "text",
                        "name": "userid",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "userid": "908f5c0d00c61bd694af4d0ef290a2cb43f3f62e5d30a6be8d89af8a026c52be",
                    "password": "f8161123d3218910426ceaf30ebeeefa3f4d94e5ae45376d2463cc42c5022b63"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "908f5c0d00c61bd694af4d0ef290a2cb43f3f62e5d30a6be8d89af8a026c52be": {
                  "label": "userid",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "f8161123d3218910426ceaf30ebeeefa3f4d94e5ae45376d2463cc42c5022b63": {
                  "label": "password",
                  "value": "YP1kIFFvluCx",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "8c3e28b4689bb89b977f75991a1c5b58565cfe1f0ee6d4233bd382a61c5ce3d4": {
                  "label": "URL",
                  "value": "http://www.lufthansa.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Mary Poppins Returns movies scripts CERT",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "50e25466ff6cd01f8d052583483cb598e01b789081a83cd8a1fdf4992f40d784": {
                  "label": "Author",
                  "value": "P.L. Travers",
                  "actionType": "NONE",
                  "hidden": false
                },
                "e53da69058e4060b1b3ece2939612e2080ff7755ae2065fc0ecebd601f54588b": {
                  "label": "Year",
                  "value": "2015",
                  "actionType": "NONE",
                  "hidden": false
                },
                "4d89a987c477cfbca80125a230ccabe5957d091252c0a1d7d60bea7dc0dbd755": {
                  "label": "Revision",
                  "value": "n.233/c",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Microsoft Office CD Key software",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "b3d7a443b8259b30d354b1b07ccb20d9d3e31bf53b1ed16639bbede1f28fd178": {
                  "label": "CD Key",
                  "value": "XE56F-TG7SE-5DWAL-KSB89-KSS9L",
                  "actionType": "NONE",
                  "hidden": false
                },
                "eea64b168177ba09b24ed192761e506a383921c8d7fad5d8241181878e31b146": {
                  "label": "prova-test",
                  "value": "filippo",
                  "actionType": "NONE",
                  "hidden": null
                },
                "3628f4301971fc43a724b18edd97d8125cb23e24a22542288ac8f73e4ffc9dd6": {
                  "label": "test-pwd",
                  "value": "8fnf3mfrpra2cwd4vuk6y y9",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Netflix entertainment",
            "data": {
              "directLogins": {
                "af97323febc46a095403cf77e9b16b0351556354aeaa4da37b17ab148d917a4a": {
                  "favicon": "http://www.netflix.com/favicon.ico",
                  "label": "Netflix",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.netflix.com/globallogin",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "authURL",
                        "value": "1425460519234.isLvWx4E7gAB8bW8eHKxS0pTgw8="
                      },
                      {
                        "type": "text",
                        "name": "email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "RememberMe",
                        "value": "on"
                      }
                    ]
                  },
                  "formValues": {
                    "RememberMe": false
                  },
                  "bindingData": {
                    "email": "487ed3fc0ed05706c845287e64f22af05b5adcf21e7797f4628b35c7c87056dc",
                    "password": "36496123b2eb199ce896fd0840ee77457b3dc08fd4539c925f7b4a610aa7dc62"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "abb418e3d92003c73df79d06b04a623cb8c1dbe4a3fead8060f121abb1d5ac3c": {
                  "label": "URL",
                  "value": "https://www.netflix.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "487ed3fc0ed05706c845287e64f22af05b5adcf21e7797f4628b35c7c87056dc": {
                  "label": "Email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "36496123b2eb199ce896fd0840ee77457b3dc08fd4539c925f7b4a610aa7dc62": {
                  "label": "Password",
                  "value": "xjnyki8yaJfNo3ROQOIInH",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "c1f7b72c69e09025e3e3e1fe2862f1d3b9d4020ee97817967415b08d1d4b315e": {
                  "label": "Customer n.",
                  "value": "846629093",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Netgear Router WNDR3700 hardware",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "85b2c982c1c847bf26de08cb75f3e7b1d3d7bae87049dee67c318ebdc03a59f6": {
                  "label": "User",
                  "value": "admin",
                  "actionType": "NONE",
                  "hidden": false
                },
                "7522900fcd696a4ee195effbe17e5524660738f1e0f69020e325a2366fe3fd9e": {
                  "label": "Password",
                  "value": "$/Mq0Q7F;?]~JUOp9=H",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "77c18f735f17a76198b9407032220e494568adb8e8ea59cbfd8bab29d15e798f": {
                  "label": "Local URL",
                  "value": "http://www.routerlogin.net/",
                  "actionType": "URL",
                  "hidden": false
                },
                "28e581da00aadc477b192f257722736ed7ce5a958aa38bfa4c2a93a58106ad27": {
                  "label": "SSID 2.4GHz",
                  "value": "joe_wifi",
                  "actionType": "NONE",
                  "hidden": false
                },
                "c31316f0a500cd40b53e390e2afa53eacfdd842d9ab6e2b27b5ed85295bef3aa": {
                  "label": "IP",
                  "value": "192.168.0.2",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "New One",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "444c3cc923a4b22bad13d22c6d14e3fe3dbdee69fb5b84d1d26463df4dae564f": {
                  "label": "",
                  "value": "",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Passport docs",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "924ca9aca7f006ff85c1007b72cfb13398595ba54d94df6fbfcbf69005560787": {
                  "label": "Passport n.",
                  "value": "A74554H334",
                  "actionType": "NONE",
                  "hidden": null
                },
                "019bbc674e5f83faa5181d194b0756fce830883e0491de3dbb794c33923e12bd": {
                  "label": "expiry date",
                  "value": "13 November 2018",
                  "actionType": "NONE",
                  "hidden": null
                },
                "197abb00c18070c2646e0a013dfed597731d72e789e7ffee63700fb4b445f25c": {
                  "label": "release date",
                  "value": "14 November 2013",
                  "actionType": "NONE",
                  "hidden": null
                },
                "6fc7ebc54467cf9fe3a127c3c61dfced8412d0c3505cfbba34fb6321d022ceb8": {
                  "label": "issued by",
                  "value": "British Embassy at Moscow",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Patent proposal",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "0951f64515cb0fa47a175b2531fc403323d6fb408e202bf9f6dccb782923f4f1": {
                  "label": "username",
                  "value": "",
                  "actionType": "NONE",
                  "hidden": false
                },
                "94475e6145a7439abf701a4011d4991c094b4f46ec1561672d84c16e4465a488": {
                  "label": "password",
                  "value": "",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "PayPal finance",
            "data": {
              "directLogins": {
                "cb5a31797a00ac70952c1e8f30679eb101353ea98313543f33caec13624b15e4": {
                  "favicon": "http://www.paypal.com/favicon.ico",
                  "label": "PayPal",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.paypal.com/signin",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "_csrf",
                        "value": "hiHzvjHOvlTgsjDY8smmAEJiFr+UGWMHoj6hw="
                      },
                      {
                        "type": "hidden",
                        "name": "locale.x",
                        "value": "it_IT"
                      },
                      {
                        "type": "email",
                        "name": "email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "bp_mid",
                        "value": "v=1;a1=na~a2=na~a3=na~a4=Mozilla~a5=Netscape~a6=5.0 (Macintosh)~a7=20100101~a8=na~a9=true~a10=Intel Mac OS X 10.10~a11=true~a12=MacIntel~a13=na~a14=Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:36.0) Gecko/20100101 Firefox/36.0~a15=true~a16=en-US~a17=na~a18=www.paypal.com~a19=na~a20=na~a21=na~a22=na~a23=1920~a24=1080~a25=24~a26=1057~a27=na~a28=Wed Mar 04 2015 09:50:45 GMT+0100 (CET)~a29=1~a30=na~a31=yes~a32=na~a33=yes~a34=no~a35=no~a36=yes~a37=no~a38=online~a39=no~a40=Intel Mac OS X 10.10~a41=no~a42=no~"
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "password": "f120d35c0f2760f2efcca92c78a2b8a0c348b3115cb7461856ccb43ea3f6c865"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "77518735ef23183fd2113a491e57885eade0cbc15c91828fa1c3dc6ad7f4f396": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "f120d35c0f2760f2efcca92c78a2b8a0c348b3115cb7461856ccb43ea3f6c865": {
                  "label": "password",
                  "value": "Gr5yCBoXZWWbd2Jpl1tUe3",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "004faba3a80b5cc64714a0fdb0af3772522352c21898d03b596b8b709151e2dc": {
                  "label": "URL",
                  "value": "https://www.paypal.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "PGP key security",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "180f593714cb20ed6802debb0630d82db7bab4c6ce73d3a08b161043216ab2b2": {
                  "label": "Email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "f240e4b1bc7a026b87064bfab1d2f5a45c11a1c5dab0fcd972382d5df6cc6ef0": {
                  "label": "Expiry date",
                  "value": "never",
                  "actionType": "NONE",
                  "hidden": false
                },
                "6bdf9ca48a5a4d05691086948c72d8906cb147d5a032d17873ec84fda115af4a": {
                  "label": "PGP key server",
                  "value": "http://pgp.mit.edu/",
                  "actionType": "URL",
                  "hidden": false
                },
                "210445b5e530d161c627cd87cca9704739e99864dac22896085d9f9dc556c60a": {
                  "label": "Key ID",
                  "value": "B8F4E7FF62AC97CD",
                  "actionType": "NONE",
                  "hidden": false
                },
                "b9be4bc78ae05fd11509617baa23cbcaf4c616b8e85a28842d00c42f63e57ca6": {
                  "label": "Fingerprint",
                  "value": "E03D 71C2 664B 1B32 123C 6F23 73F2 E8FB 62CC 67CD",
                  "actionType": "NONE",
                  "hidden": false
                },
                "929428dc1bd254ccfb5fabe2bafeef9445c9069ba806d903778c36d501ad7e53": {
                  "label": "Algorithm",
                  "value": "RSA 4096",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Prototype 34/62 - v.8 ",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "318d22b8a6fd8da7a6e29c2991fe1eb265cff3b20ca447d21694469c42d5cf2b": {
                  "label": "Project codename",
                  "value": "Wonder planet",
                  "actionType": "NONE",
                  "hidden": false
                },
                "1f412d25e33583f8e737408cbfa9d7155539e9b38e487ef74995aa852af0d838": {
                  "label": "Last update",
                  "value": "12 February 2016",
                  "actionType": "NONE",
                  "hidden": false
                },
                "383fd8ca69e7582eece74b9fcf06b34dc1bec8d64e06428addc2f2db5a5b39ca": {
                  "label": "Related patents",
                  "value": "466388, 663382, 993744 ",
                  "actionType": "NONE",
                  "hidden": null
                }
              }
            }
          },
          {
            "label": "Raspberry Pi hardware",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "19b66fa0b2d839e4578339e642442fa1aa82b5b33e12e482ba0cf6c11545c24f": {
                  "label": "user",
                  "value": "pi",
                  "actionType": "NONE",
                  "hidden": false
                },
                "d9e8ff9a8d125f27f723f649341029b0b2778b0927a3f02d02c7368b82a5f061": {
                  "label": "password",
                  "value": "gdC=hs_{XE*N;]f~1nu",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "2a09e77c2e5bd16a768dd2f2f31f3611867d9311bcae09127852920066b37d4f": {
                  "label": "hostname",
                  "value": "joe_pi",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Spotify entertainment",
            "data": {
              "directLogins": {
                "aebadbfcc69bf029b2ae47cfb4e3bc2b8b71ae4a90f786538dcb44be6b5fc1f9": {
                  "favicon": "http://www.spotify.com/favicon.ico",
                  "label": "Spotify",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.spotify.com/it/xhr/json/login.php",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "sp_csrf",
                        "value": "eyJzaWduIjoiNmMzYjE2ZjlmNmJkMmZiMmNlOGE1NzUzNTE2MjljMzZkMDZmOWJmMyIsImRhdGEiOjE0MjU0NjAzMzF9"
                      },
                      {
                        "type": "hidden",
                        "name": "forward_url",
                        "value": "/it/account/overview/"
                      },
                      {
                        "type": "hidden",
                        "name": "referrer",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "utm-keywords",
                        "value": "19f202a2d6fbfc1c801e79cb09e3cc5d"
                      },
                      {
                        "type": "text",
                        "name": "user_name",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "password",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "user_name": "775c5c0f5c72631d4201889794ea069ed4e4db6a1516191093da6cfdf633cd79",
                    "password": "ccede6361327414def96cba8e6455508e0427e7db69391b86295ccb8bfd89132"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "775c5c0f5c72631d4201889794ea069ed4e4db6a1516191093da6cfdf633cd79": {
                  "label": "user_name",
                  "value": "joe.clipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "ccede6361327414def96cba8e6455508e0427e7db69391b86295ccb8bfd89132": {
                  "label": "password",
                  "value": "ljOKAkro3vq8pjDiemaniI",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "d86bd2deec89224d79522c3f10217691b012dcafb220ac5809c5b339689faf4f": {
                  "label": "URL",
                  "value": "https://www.spotify.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "T-Online.de family",
            "data": {
              "directLogins": {
                "9a8e6cc57266b8364547f450e71d98596efd9210df8f5f1a9dc755df0b23f0db": {
                  "favicon": "http://login.idm.telekom.com/favicon.ico",
                  "label": "My Login",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://login.idm.telekom.com/login",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": "domn",
                        "value": "t-online.de"
                      },
                      {
                        "type": "hidden",
                        "name": "tid",
                        "value": "wzb9r458N1HxwX0gUGrNAahw"
                      },
                      {
                        "type": "hidden",
                        "name": "domn",
                        "value": "t-online.de"
                      },
                      {
                        "type": "hidden",
                        "name": "appid",
                        "value": "0848"
                      },
                      {
                        "type": "hidden",
                        "name": "skinid",
                        "value": "35"
                      },
                      {
                        "type": "hidden",
                        "name": "lang",
                        "value": "en"
                      },
                      {
                        "type": "text",
                        "name": "usr",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "pwd",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "usr": "c03dd16142d5f7423747c70569382af9b8f6df1e9f5fef11745770dca173bfe8",
                    "pwd": "e10024c998661241f93cb4a2cae79736cb5bb3506a2689ce1677a162fffdc7f4"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "80279c7b31b978fc31886c982b5dd0fea1074bc66fc6237d895ea225ea40bfab": {
                  "label": "Web address",
                  "value": "http://telekom.de",
                  "actionType": "URL",
                  "hidden": false
                },
                "c03dd16142d5f7423747c70569382af9b8f6df1e9f5fef11745770dca173bfe8": {
                  "label": "Username or email",
                  "value": "jclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "e10024c998661241f93cb4a2cae79736cb5bb3506a2689ce1677a162fffdc7f4": {
                  "label": "Password",
                  "value": "+\\|3n^#ykqRaCgr<dRt",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Taxi Driver 2 scripts movies",
            "data": {
              "directLogins": {
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "7c4499ff09964bb098e4a2d3d2143d3ac605756a281e38c1a7c029b5aec6ae78": {
                  "label": "Author",
                  "value": "Herman Melville",
                  "actionType": "NONE",
                  "hidden": false
                },
                "553d0f0089d689ddd09f1deb507033b9a456f2ad7720887a0ec479603dfecb0b": {
                  "label": "Completion date",
                  "value": "July 26, 2015",
                  "actionType": "NONE",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "The New York Times reading",
            "data": {
              "directLogins": {
                "f0d93bf3e52bc4f4e8d34c4759f5f566c1bcf220bcf5edd28af71ee8ef0da8dd": {
                  "favicon": "http://international.nytimes.com/favicon.ico",
                  "label": "The New York Times - Breaking News, World News & Multimedia",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "http://international.nytimes.com/",
                      "method": null
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "login-email",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "login-password",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "login-remember",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                    "login-remember": false
                  },
                  "bindingData": {
                    "login-email": "e432656d07b4f42e50769da25171fe145458fda2fd5890fe4c7994404bfaf887",
                    "login-password": "c596e4ba1000543ea6eefb12d521b8e9560c9b23ff6b948a386b67feb0c2788b"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "e432656d07b4f42e50769da25171fe145458fda2fd5890fe4c7994404bfaf887": {
                  "label": "user ID",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "c596e4ba1000543ea6eefb12d521b8e9560c9b23ff6b948a386b67feb0c2788b": {
                  "label": "password",
                  "value": "bcvsm6s53pknsw",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "f8cc7b66220085430080494e4e3c59c1430ef802d418266d54be1dc4ea33f0a3": {
                  "label": "URL",
                  "value": "http://www.nytimes.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Tumblr social",
            "data": {
              "directLogins": {
                "53baf661f0c41b41026cd8e8cc4dd1855956461b9201286cb98486e4afb8254b": {
                  "favicon": "http://www.tumblr.com/favicon.ico",
                  "label": "Tumblr",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.tumblr.com/login",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "email",
                        "name": "user[email]",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "user[password]",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "tumblelog[name]",
                        "value": ""
                      },
                      {
                        "type": "text",
                        "name": "user[age]",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "user[tos]",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "context",
                        "value": "no_referer"
                      },
                      {
                        "type": "hidden",
                        "name": "version",
                        "value": "STANDARD"
                      },
                      {
                        "type": "hidden",
                        "name": "follow",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "form_key",
                        "value": "!1231425459636|mN1XwAQcG07jDch2e8I1dZac0lg"
                      },
                      {
                        "type": "hidden",
                        "name": "seen_suggestion",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": "used_suggestion",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": "used_auto_suggestion",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": "about_tumblr_slide",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "random_username_suggestions",
                        "value": "[\"BrieflyDelicatePatrol\",\"EclecticKingCrown\",\"HardTastemakerBarbarian\",\"StrangePhantomBlaze\",\"WeepingWonderlandDinosaur\"]"
                      }
                    ]
                  },
                  "formValues": {
                    "user[tos]": false
                  },
                  "bindingData": {
                    "user[password]": "e094a5d97b5e2a4ab2274a2c2e9d0efa4b5f2cf80d2bb1b44c91d46137def6d2",
                    "tumblelog[name]": "null",
                    "user[age]": "null"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "4a9159e2b9be049f1f192852cd72ec6e05f6c2aff2aa50e6c9f60123d41de9b1": {
                  "label": "Web address",
                  "value": "https://www.tumblr.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "a424da441206bebc8786d78af1475dcd4dad0b83e85df708a9dbef02faa197b7": {
                  "label": "Email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "e094a5d97b5e2a4ab2274a2c2e9d0efa4b5f2cf80d2bb1b44c91d46137def6d2": {
                  "label": "Password",
                  "value": "WZPKMUuucgpOesPHKtfxxf",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Twitter social",
            "data": {
              "directLogins": {
                "8d7bf22dbce3e7d5eb873fa437c3b27168afa8bd28d06fec76026f9e9dd8c26c": {
                  "favicon": "http://twitter.com/favicon.ico",
                  "label": "Joe's Twitter",
                  "bookmarkletVersion": "0.3.0",
                  "formData": {
                    "attributes": {
                      "action": "https://twitter.com/sessions?phx=1",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "session[username_or_email]",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "session[password]",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": "remember_me",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": "scribe_log",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "redirect_after_login",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "session[username_or_email]": "14eba71f94274158e7656d5412cb95b7047e474e484118fe6c7d8ccc558ced9f",
                    "session[password]": "47e39d4cee22bff9f3b6b62c4676a41603a68c6d3ba1ba3745df7f314351684e"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "0ea4aceaecc589ac624ec16650c621412b3ce77003784417f25af1fd6d78f17c": {
                  "label": "URL",
                  "value": "http://www.twitter.com",
                  "actionType": "URL",
                  "hidden": false
                },
                "8e32383ccd565b8d0698bf3b628a1e0e5f4ef5164239ee96c464e741e8e4831a": {
                  "label": "email",
                  "value": "joe@clipperz.com",
                  "actionType": "EMAIL",
                  "hidden": false
                },
                "14eba71f94274158e7656d5412cb95b7047e474e484118fe6c7d8ccc558ced9f": {
                  "label": "username",
                  "value": "JoeClipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "47e39d4cee22bff9f3b6b62c4676a41603a68c6d3ba1ba3745df7f314351684e": {
                  "label": "password",
                  "value": "bvscfazncwpetsdgs",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          },
          {
            "label": "Uniqlo shopping",
            "data": {
              "directLogins": {
                "34e2981a6cf30b15b774ba76a7de32b83795c95f4ea04b49e34ba7d85e77c7f2": {
                  "favicon": "http://www.uniqlo.com/favicon.ico",
                  "label": "Uniqlo",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://www.uniqlo.com/uk/store/FSC05010E01.do?r=6173033985359397408",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "text",
                        "name": "cust_eml_id",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "cust_pass",
                        "value": ""
                      }
                    ]
                  },
                  "formValues": {
                  },
                  "bindingData": {
                    "cust_eml_id": "56e51df0e9889057ef59b2bf7ddee39e059ab4330813024c92257dafa1506402",
                    "cust_pass": "1ccf701a3880e19d2a2583b4390e170573f2892d193a1c5e465edb5ca566e948"
                  }
                }
              },
              "notes": ""
            },
            "currentVersion": {
              "fields": {
                "56e51df0e9889057ef59b2bf7ddee39e059ab4330813024c92257dafa1506402": {
                  "label": "USERNAME",
                  "value": "joeclipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "1ccf701a3880e19d2a2583b4390e170573f2892d193a1c5e465edb5ca566e948": {
                  "label": "PASSWORD",
                  "value": "uL/kVNBKy>XtLNzgYs*",
                  "actionType": "PASSWORD",
                  "hidden": true
                },
                "9ee688ef4e91d32968838717051551464e91c8bd589b3de8d08af582edebcd36": {
                  "label": "URL",
                  "value": "https://www.uniqlo.com",
                  "actionType": "URL",
                  "hidden": false
                }
              }
            }
          },
          {
            "label": "Yahoo! entertainment",
            "data": {
              "directLogins": {
                "dba0db679802f0e6aa6d0b7a6aaf42350aabc5f057409edd99a268a92ebb6496": {
                  "favicon": "http://login.yahoo.com/favicon.ico",
                  "label": "Yahoo! Mail",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://login.yahoo.com/config/login?",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": ".tries",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": ".src",
                        "value": "ym"
                      },
                      {
                        "type": "hidden",
                        "name": ".md5",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".hash",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".js",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".last",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "promo",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".intl",
                        "value": "us"
                      },
                      {
                        "type": "hidden",
                        "name": ".bypass",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".partner",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".u",
                        "value": "5gp62cl2vg3ov"
                      },
                      {
                        "type": "hidden",
                        "name": ".v",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".challenge",
                        "value": "iBEY0IK6k3t9Uals32mrTos8s48p"
                      },
                      {
                        "type": "hidden",
                        "name": ".yplus",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".emailCode",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "pkg",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "stepid",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".ev",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "hasMsgr",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".chkP",
                        "value": "Y"
                      },
                      {
                        "type": "hidden",
                        "name": ".done",
                        "value": "http://mail.yahoo.com"
                      },
                      {
                        "type": "hidden",
                        "name": ".pd",
                        "value": "ym_ver%3d0%26c="
                      },
                      {
                        "type": "text",
                        "name": "login",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "passwd",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": ".persistent",
                        "value": "y"
                      },
                      {
                        "type": "submit",
                        "name": ".save",
                        "value": "Sign In"
                      }
                    ]
                  },
                  "formValues": {
                    ".persistent": false
                  },
                  "bindingData": {
                    "login": "4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9",
                    "passwd": "ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b"
                  }
                },
                "aa18149164302d5dbe7e2d3724565b9550e00887b49978559783b2e38c625584": {
                  "favicon": "http://login.yahoo.com/favicon.ico",
                  "label": "Yahoo! Groups",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://login.yahoo.com/config/login?",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": ".tries",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": ".src",
                        "value": "ygrp"
                      },
                      {
                        "type": "hidden",
                        "name": ".md5",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".hash",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".js",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".last",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "promo",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".intl",
                        "value": "us"
                      },
                      {
                        "type": "hidden",
                        "name": ".bypass",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".partner",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".u",
                        "value": "8if56nd2vg3um"
                      },
                      {
                        "type": "hidden",
                        "name": ".v",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".challenge",
                        "value": "F.8L0tZzk3vMDAaEG.GPr.qbZIUT"
                      },
                      {
                        "type": "hidden",
                        "name": ".yplus",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".emailCode",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "pkg",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "stepid",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".ev",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "hasMsgr",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".chkP",
                        "value": "Y"
                      },
                      {
                        "type": "hidden",
                        "name": ".done",
                        "value": "http://groups.yahoo.com/"
                      },
                      {
                        "type": "hidden",
                        "name": ".pd",
                        "value": "ygrp_ver%3d0%26c="
                      },
                      {
                        "type": "text",
                        "name": "login",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "passwd",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": ".persistent",
                        "value": "y"
                      },
                      {
                        "type": "submit",
                        "name": ".save",
                        "value": "Sign In"
                      }
                    ]
                  },
                  "formValues": {
                    ".persistent": false
                  },
                  "bindingData": {
                    "login": "4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9",
                    "passwd": "ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b"
                  }
                },
                "6f7bbc4e42ea462b5246e6f51c3f86056bec50601ce2de6067c8c1d26f21c07f": {
                  "favicon": "http://login.yahoo.com/favicon.ico",
                  "label": "Flickr",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://login.yahoo.com/config/login?",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": ".tries",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": ".src",
                        "value": "flkctx"
                      },
                      {
                        "type": "hidden",
                        "name": ".md5",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".hash",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".js",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".last",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "promo",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".intl",
                        "value": "us"
                      },
                      {
                        "type": "hidden",
                        "name": ".bypass",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".partner",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".u",
                        "value": "25v75kt2vg8id"
                      },
                      {
                        "type": "hidden",
                        "name": ".v",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".challenge",
                        "value": "d2roZizovnunje.COLRyi3FhseYs"
                      },
                      {
                        "type": "hidden",
                        "name": ".yplus",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".emailCode",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "pkg",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "stepid",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".ev",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "hasMsgr",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".chkP",
                        "value": "Y"
                      },
                      {
                        "type": "hidden",
                        "name": ".done",
                        "value": "https://login.yahoo.com/config/validate?.src=flkctx&.pc=5134&.done=http%3A%2F%2Fwww.flickr.com%2Fsignin%2Fyahoo%2F"
                      },
                      {
                        "type": "hidden",
                        "name": ".pd",
                        "value": "flkctx_ver%3d0%26c="
                      },
                      {
                        "type": "text",
                        "name": "login",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "passwd",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": ".persistent",
                        "value": "y"
                      },
                      {
                        "type": "submit",
                        "name": ".save",
                        "value": "Sign In"
                      }
                    ]
                  },
                  "formValues": {
                    ".persistent": false
                  },
                  "bindingData": {
                    "login": "4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9",
                    "passwd": "ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b"
                  }
                },
                "a7b32e72502804bf2946a2a8856139cbbb759c5777e6b3e673db1fdf7e3bd06e": {
                  "favicon": "http://login.yahoo.com/favicon.ico",
                  "label": "My Yahoo!",
                  "bookmarkletVersion": "0.2",
                  "formData": {
                    "attributes": {
                      "action": "https://login.yahoo.com/config/login?",
                      "method": "post"
                    },
                    "inputs": [
                      {
                        "type": "hidden",
                        "name": ".tries",
                        "value": "1"
                      },
                      {
                        "type": "hidden",
                        "name": ".src",
                        "value": "fpctx"
                      },
                      {
                        "type": "hidden",
                        "name": ".md5",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".hash",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".js",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".last",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "promo",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".intl",
                        "value": "us"
                      },
                      {
                        "type": "hidden",
                        "name": ".bypass",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".partner",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".u",
                        "value": "ebtv9tt32a0au"
                      },
                      {
                        "type": "hidden",
                        "name": ".v",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".challenge",
                        "value": "XC3tzcf7nab_aYGAfZqgnxDVFOIH"
                      },
                      {
                        "type": "hidden",
                        "name": ".yplus",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".emailCode",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "pkg",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "stepid",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": ".ev",
                        "value": ""
                      },
                      {
                        "type": "hidden",
                        "name": "hasMsgr",
                        "value": "0"
                      },
                      {
                        "type": "hidden",
                        "name": ".chkP",
                        "value": "Y"
                      },
                      {
                        "type": "hidden",
                        "name": ".done",
                        "value": "http://www.yahoo.com"
                      },
                      {
                        "type": "hidden",
                        "name": ".pd",
                        "value": "fpctx_ver%3d0%26c="
                      },
                      {
                        "type": "text",
                        "name": "login",
                        "value": ""
                      },
                      {
                        "type": "password",
                        "name": "passwd",
                        "value": ""
                      },
                      {
                        "type": "checkbox",
                        "name": ".persistent",
                        "value": "y"
                      },
                      {
                        "type": "submit",
                        "name": ".save",
                        "value": "Sign In"
                      }
                    ]
                  },
                  "formValues": {
                    ".persistent": false
                  },
                  "bindingData": {
                    "login": "4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9",
                    "passwd": "ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b"
                  }
                }
              },
              "notes": "--"
            },
            "currentVersion": {
              "fields": {
                "4cfaf1e782086e7527bd0e0cc82b67eb773e8157ad0c5babe516f7bc945a02a9": {
                  "label": "userID",
                  "value": "joe.clipperz",
                  "actionType": "NONE",
                  "hidden": false
                },
                "ef2dee54322bf401540657d469e158a50e9228bc0a192a31d2e3ee56a77e565b": {
                  "label": "password",
                  "value": "enfvDG1RxAsl",
                  "actionType": "PASSWORD",
                  "hidden": true
                }
              }
            }
          }
        ]
      """
