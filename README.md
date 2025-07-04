# Pedestrian Simulation in Retail Environments During COVID-19

This repository contains the R code used for my Master's thesis, which focused on simulating pedestrian movement in supermarkets during the COVID-19 pandemic. The project explored how different supermarket layouts and entry policies affect social distancing, crowding, and movement efficiency. The simulation was designed to help retailers understand and optimize store setups to support safer shopping experiences.

## 🧠 Project Highlights

- Agent-based modeling of individual pedestrian behavior
- Configurable store layouts and entry strategies
- Simulation of social distancing interactions (e.g., passing, queuing)
- Comparative scenarios for layout efficiency and safety trade-offs
- Thesis awarded **9/10** for originality and practical relevance

## 📁 Project Structure

- `PredictivePedestrianA7.R` – Main model containing core simulation logic
- `ExecuteSim.R` – Entry script to run simulation experiments
- `playRetail*.R` – Scenario-specific simulations (e.g., middle aisle, all-through)
- `conflictingFunc.R` – Helper or utility functions used in multiple runs

## 🚀 Getting Started

### Requirements
- R (version 4.0+ recommended)
- Required packages: `ggplot2`, `dplyr`, `gridExtra`, `reshape2`, `data.table`

### Run a Simulation
```r
source("ExecuteSim.R")
