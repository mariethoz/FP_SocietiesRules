# 🦠 Pandemia Behavior Analysis

It is a ScalaFX-based simulation that models the behavioral evolution of a population in response to a virus outbreak. It visualizes how individuals' mindset (*Comply*, *Neutral*, *Reject*) changes over time, depending on population size, peer influence, and exposure to infection — even in the absence of actual infections.

---

## Key Concepts

- **Behavior States**:
    - `Comply`: Follows health rules (e.g., masks, distancing)
    - `Neutral`: Undecided; swayed by peer influence
    - `Reject`: Actively disregards rules

- **Health States**:
    - `Healthy`: People that could contract the infection
    - `Infected`: People infected that spread the infection
    - `Recovered`: People who were infected, cannot spread or contract the infection
    - Used to simulate how fear or safety affect mindset.

- **Payoff-Based Transitions:**
    - Each individual observes neighbors and adjusts their **mindset**
    - A configurable **payoff function** governs this transition based on local social and health context

---


```bash
src/main/scala/
  Behavior/
  ├── BehaviorPropagationExample.scala  # Simulation logic and models for behavior
  ├── BehaviorSimGUI.scala              # An example for a Behavior visualisation app
  Pandemia/
  ├── PandemiaPropagationExample.scala  # Simulation logic and models combining behavior and infection propagation
  ├── PandemiaSimGUI.scala              # An example for a Pandemia visualization app
  ├── PandemiaSimPlot.scala             # An example to plot graphs for a Pandemia simulation
  ├── PandemiaSim.scala                 # This Pandemia visualization app
  ├── PandemiaTransition.scala          # It is an example for the metric
  Virus/
  ├── VirusPropagationExample.scala     # Simulation logic and models for virus propagation
  ├── VirusSimGUI.scala                 # An example for a Virus visualisation app
```


## Pandemia
### PandemiaPropagationExample: Base logic
#### Overview
This file contains the main element and functions related to agents (`Person`):
- `BeahviorStatus`: 
  - `Comply`: the person follow the rules
  - `Neutral`: the person follow the rules but do not fully apply theme
  - `Reject`: the person does not follow the rules
- `HealthStatus`:
  - `Healthy`: the person is in good health and can contract the infection
  - `Infected`: the person has contracted the infection and currently spread it
  - `Recovered`: the person had contracted the infection and cannot have or spread it
- `observation`: compute the mind score according to the surrounding
- `updateMindset`: update the behavior according to the mind score
- `move`: move the agents into the limitation of the word
- `infect`: simulate the health part of the agents including the infection chance related to the surrounding
- `populationVector`: return a `Vector` of `Person` according to the input

#### Behavior Influence Logic

The function `observation` models how individuals update their mindset (`Comply`, `Neutral`, `Reject`) by evaluating their social and health environment. The payoff they receive from nearby individuals determines whether they become compliant, rebellious, or neutral.

##### Behavior Rules by Mindset

- `Comply`
  - Rewards proximity to other compliers.
  - A bit of proud around neutrals.
  - Penalized heavily by rejects.
  - If health state is considered:
    - Infected neighbors add fear → higher payoff for staying compliant.
    - Recovered neighbors reduce fear → reduce payoff for staying compliant.

- `Neutral`
  - Driven by the majority behavior nearby.
  - Gets boosted by conforming to the local majority (`Comply` or `Reject`). 
  - Opposition by `Neutral`. 
  - If health state is considered:
    - Infected neighbors increase anxiety.
    - Recovered neighbors reduce fear.

- `Reject` 
  - If compliant people around:
    - They are inspired by compliers.
    - They fear other rejects.
  - If no compliant people around:
    - They reinforce their behavior
  - Reinforced by neutral behaviors (ambiguity).
  - If health state is considered:
    - Rejection is weakened when infection is visible.
    - Fear is reduced if many people are healthy or recovered.
 
##### Payoff Scoring (Numeric Weights)

| Mindset  | Near Comply           | Near Neutral                             | Near Reject                     | Near Infected | Near Healthy | Near Recovered |
|----------|-----------------------|------------------------------------------|---------------------------------|---------------|--------------|----------------|
| Comply   | 2                     | 1                                        | −5                              | 2             | —            | −1             |
| Neutral  | 7 (if majority)       | (+ if majority complier around else -) 5 | 7 (if majority)                 | 2             | —            | −2             |
| Reject   | 5 (if Comply present) | −5             7                         | (+ if complier around else -) 2 | 2             | −1           | −2             |

### PandemiaSim: Interactive Simulation Interface

This GUI application visualizes the co-evolution of virus propagation and individual behavioral adaptation in a population. It is built with **ScalaFX** and features real-time simulation, plotting, and full user control over key parameters.

#### 🔧 Parameters

You can configure all the parameters before starting or resetting the simulation:

| Input                     | Description                                           | Default |
|---------------------------|-------------------------------------------------------|---------|
| **Population Size**       | Number of agents in the simulation                    | 150     |
| **Initial Infected**      | How many start as infected                            | 1       |
| **Initial Complying**     | Number of agents starting with `Comply` behavior      | 50      |
| **Initial Rejecting**     | Number of agents starting with `Reject` behavior      | 50      |
| **Infection Radius**      | How far infection can spread                          | 3       |
| **Observation Radius**    | How far agents observe others to change behavior      | 5       |
| **Infection Chance (%)**  | Chance of transmission upon contact                   | 90      |
| **Speed (ms)**            | Time between updates in milliseconds                  | 200     |
| **Health Influence**      | Toggle to consider health status in behavior dynamics | off     |


#### Mind & Health Visualization

Each agent is visualized on a canvas using:

- **Rectangle color** = **Mindset**
  - 🟩 Green: `Comply`
  - 🟧 Orange: `Neutral`
  - 🟥 Red: `Reject`
  - 
- **Circle color** = **Health**
  - 🟢 Green: `Healthy`
  - 🔴 Red: `Infected`
  - 🔵 Blue: `Recovered`


- Real-time Charts:
  1. Health states: `Healthy`, `Infected`, `Recovered`
  2. Behavior states: `Comply`, `Neutral`, `Reject`


- Two horizontal bars display live statistics:
  1. **Mindset Counters:**  
     `Comply / Start` – `Neutral / Start` – `Reject / Start`
  2. **Health Counters:**  
     `Healthy / Start` – `Infected / Start` – `Recovered / Start`


#### ️ Controls

| Control Button       | Function                                       |
|----------------------|------------------------------------------------|
| **Start/Pause**      | Begins or pauses the simulation                |
| **Reset**            | Recreates the population and resets graphs     |
| **Health Influence** | Enables health-driven behavior switching       |


#### ️ Simulation Logic

Each simulation **step** updates the system as follows:

1. **Movement** — All agents move slightly within bounds.
2. **Infection** — `Infected` agents may infect nearby `Healthy` agents.
3. **Observation** — Agents count neighbors and update mindset scores.
4. **Mindset Update** — Agents switch between `Comply`, `Neutral`, `Reject`.
5. **Display & Charts** — Updated canvas and time-series plots.

---

### PandemiaTransition
#### What the Simulation Shows

For population sizes ranging from **100** to **1000**, the app:

- Runs multiple independent trials per population size (e.g. 10 simulations)
- Evolves each population over a fixed number of steps (e.g. 1000)
- Computes the average final proportions of `Comply`, `Neutral`, and `Reject`

#### Output

- A **stacked area chart** shows how population size affects mindset distribution.
- You can **save the chart** as a PNG file.
- Console logs display the numeric results.

