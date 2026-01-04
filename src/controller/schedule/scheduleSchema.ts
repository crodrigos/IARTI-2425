let ScheduleSchema : {
    vessels: [{
        ref: string,
        arrivalTime: number
        departureTime: number,
        unloadingTime: number,
        loadingTime: number
    }],
    heuristic?: {
        type: "EAT" | "EDT" | "SOT"
    }
    genetic?: {
        populationSize: number,
        maxGenerations: number,
        crossoverProbabilty: number,
        mutationProbabilty: number,
        previousGenerationLength: number,
        stagnation_margin: number,
    }
}