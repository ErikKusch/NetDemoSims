Network_mat <- Sim.Network(n_spec = 10, NetworkType = "Association", Sparcity = 0.5, MaxStrength = 1, seed = 42)
use_data(Network_mat)

CarryingK_vec <- Sim.CarryingK(n_spec = 10, k_range = c(200, 200), seed = 42)
use_data(CarryingK_vec)

Niches_vec <- Sim.Niche(n_spec = 10, Env_range = c(0, 10), seed = 42)
use_data(Niches_vec)

Initialise_df <- Sim.Initialise(
    n_spec = 10, n_individuals = 4e2, n_mode = "total", Env_range = c(0, 10),
    Trait_means = Niches_vec, Trait_sd = 1, seed = 42
)
use_data(Initialise_df)

Env_mat <- Sim.Space(
    x_range = c(0, 10), y_range = c(0, 10),
    ncol = 1e3, nrow = 1e3,
    x_gradient = function(x) x,
    y_gradient = function(y) y
)
use_data(Env_mat)

SimulationOutput <- Sim.Compute(
    # Demographic parameters
    d0 = 0.4,
    b0 = 0.6,
    k_vec = CarryingK_vec,
    ID_df = Initialise_df,
    # Spatial parameters
    env.xy = Env_mat,
    env.sd = 2.5,
    mig.sd = 0.2,
    mig.top = 0.05,
    # Interaction parameters
    interac.maxdis = 0.5,
    interac.mat = Network_mat,
    interac.scale = 1,
    # Simulation parameters
    Sim.t.max = 5,
    Sim.t.inter = 0.1,
    seed = 42,
    verbose = TRUE, # whether to print progress in time as current time
    RunName = "Trial"
)
use_data(SimulationOutput)

Inferred_mat <- Sim.Network(n_spec = 10, NetworkType = "Association", Sparcity = 0.5, MaxStrength = 1, seed = 43)
use_data(Inferred_mat)
