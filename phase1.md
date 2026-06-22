## Phase 1 — Coverage estimation + visualization

### Step 1 — Explore & clean  *(Python)*
- Profile `all-schools.csv` (done: grades K–5, risk tiers, 132 `no_data`
  schools, 24% missing geocodes, cross-state `school_id` collisions).
- Subset to **one county** (good size + completeness) for the first end-to-end run.
- Output: a clean, documented per-school × per-grade long table.

### Step 2 — Build imuGAP inputs  *(Python)*
- `melt` wide grades → long; build the **three tables** per
- Resolve open decisions (survey year; per-grade `sample_n` via CI-derived
  effective n; dose = 2-dose MMR).
- Save to `data/processed/`.

### Step 3 — Validate  *(R, ~10 lines)*
- `canonicalize_locations/observations/populations()` = **schema gate**; fix any
  errors before paying for a fit.

### Step 4 — Fit  *(R / imuGAP)*
- `sampling(observations, populations, locations, stan_opts = stan_options(...))`
  on the one-county subset (fast).
- **Check convergence — read these like training diagnostics:**
  - **R-hat ≈ 1** → the chains/seeds agree (converged). >1.01 = not converged.
  - **ESS** = effective # of independent samples (autocorrelation-discounted) —
    low ESS ≈ a tiny effective batch, estimates noisy.
  - **divergences** ≈ NaN/exploding-gradient warnings → reparametrize / raise
    `adapt_delta`.
  - **trace plot** = the "loss curve" you eyeball for a stationary, well-mixed band.

### Step 5 — Predict & impute  *(R / imuGAP)*
- `create_target()` for: all schools (incl. the 132 `no_data`), all grades/ages,
  dose 2, plus county/state roll-ups.
- `predict()` → `summary()` → coverage **mean + 95% credible interval** per target.

### Step 6 — Visualize  *(Python, from the predictions)*
- Coverage-by-age curves (school/county/state) with uncertainty ribbons.
- **Maps** (lon/lat) of estimated coverage and risk tier.
- School-coverage distributions; highlight imputed `no_data` schools.
- Imputed estimates vs. the CSV's own `coverage`/`tier`/`prob_below_95`.

### Step 7 — Scale out
- Re-run the validated pipeline on a **full state** (CA or NC), then both. Watch
  MCMC runtime; partial pooling carries the full hierarchy.

### Step 8 — Report  *(Python notebook / Markdown)*
- Methods, the data→imuGAP mapping, results, figures, limitations.