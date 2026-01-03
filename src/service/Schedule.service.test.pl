:- use_module([
    library(http/http_server),
    library(http/http_header),
    library(http/http_client),
    library(http/http_json),
    library(http/json),
    library(debug),
    "./Schedule.service.pl"
]).

test:-
    debug(_),
    JSONDict = _{
        genetic: _{
            maxgenerations: 100
        },
        docks: [2,3,4],
        vessels: [
            _{
            ref: "Zeus",
            arrivalTime: 6,
            departureTime: 63,
            unloadingTime: 10,
            loadingTime: 16
            },
            _{
            ref: "Poseidon",
            arrivalTime: 23,
            departureTime: 50,
            unloadingTime: 9,
            loadingTime: 7
            },
            _{
            ref: "Graca",
            arrivalTime: 8,
            departureTime: 40,
            unloadingTime: 5,
            loadingTime: 12
            },
            _{
            ref: "Marques",
            arrivalTime: 10,
            departureTime: 30,
            unloadingTime: 0,
            loadingTime: 8
            },
            _{
            ref: "Onda",
            arrivalTime: 36,
            departureTime: 70,
            unloadingTime: 12,
            loadingTime: 0
            },
            _{
            ref: "Cartografo",
            arrivalTime: 15,
            departureTime: 55,
            unloadingTime: 8,
            loadingTime: 10
            },
            _{
            ref: "Dona Maria",
            arrivalTime: 28,
            departureTime: 65,
            unloadingTime: 7,
            loadingTime: 9
            },
            _{
            ref: "Cacador",
            arrivalTime: 45,
            departureTime: 80,
            unloadingTime: 6,
            loadingTime: 11
            },
            _{
            ref: "marenostrum",
            arrivalTime: 8,
            departureTime: 50,
            unloadingTime: 8,
            loadingTime: 14
            },
            _{
            ref: "nautilus",
            arrivalTime: 9,
            departureTime: 45,
            unloadingTime: 6,
            loadingTime: 12
            },
            _{
            ref: "floating",
            arrivalTime: 12,
            departureTime: 60,
            unloadingTime: 9,
            loadingTime: 10
            },
            _{
            ref: "atlantis",
            arrivalTime: 14,
            departureTime: 70,
            unloadingTime: 11,
            loadingTime: 11
            },
            _{
            ref: "odyssey",
            arrivalTime: 15,
            departureTime: 75,
            unloadingTime: 8,
            loadingTime: 9
            },
            _{
            ref: "triton",
            arrivalTime: 18,
            departureTime: 80,
            unloadingTime: 9,
            loadingTime: 13
            },
            _{
            ref: "neptune",
            arrivalTime: 20,
            departureTime: 85,
            unloadingTime: 10,
            loadingTime: 10
            },
            _{
            ref: "aquarius",
            arrivalTime: 22,
            departureTime: 90,
            unloadingTime: 7,
            loadingTime: 8
            },
            _{
            ref: "aurora",
            arrivalTime: 25,
            departureTime: 88,
            unloadingTime: 6,
            loadingTime: 9
            },
            _{
            ref: "chronos",
            arrivalTime: 26,
            departureTime: 95,
            unloadingTime: 10,
            loadingTime: 12
            },
            _{
            ref: "argo",
            arrivalTime: 27,
            departureTime: 92,
            unloadingTime: 11,
            loadingTime: 10
            },
            _{
            ref: "icarus",
            arrivalTime: 30,
            departureTime: 100,
            unloadingTime: 12,
            loadingTime: 12
            },
            _{
            ref: "titan",
            arrivalTime: 32,
            departureTime: 102,
            unloadingTime: 13,
            loadingTime: 11
            },
            _{
            ref: "leviathan",
            arrivalTime: 35,
            departureTime: 110,
            unloadingTime: 10,
            loadingTime: 10
            },
            _{
            ref: "hydra",
            arrivalTime: 38,
            departureTime: 112,
            unloadingTime: 8,
            loadingTime: 14
            },
            _{
            ref: "hermes",
            arrivalTime: 40,
            departureTime: 115,
            unloadingTime: 9,
            loadingTime: 13
            },
            _{
            ref: "phoenix",
            arrivalTime: 42,
            departureTime: 118,
            unloadingTime: 10,
            loadingTime: 10
            },
            _{
            ref: "selene",
            arrivalTime: 45,
            departureTime: 120,
            unloadingTime: 7,
            loadingTime: 9
            }
        ]
    },
    getSchedule(JSONDict, ScheduleDict, Delay, TimeTaken).