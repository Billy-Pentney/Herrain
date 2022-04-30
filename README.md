# Herrain

## Description
Herrain uses three layers of Perlin-Noise to generate maps for the following attributes:
  - Temperature (T)
  - Precipitation (P)
  - Elevation (E)

Then, the map is built on a grid-system, where the biome of each cell is determine by the combination of noise values at those coordinates.
These biomes can then be visualised using Gloss.

## Biomes
- Ice = Low/Medium Elevation + Low Temperature
- Tundra = High Elevation + Low Temperature
- Rocky = High Elevation + Medium/High Temperature
- Ocean/Water/Coast = Increasing Elevation + Medium Temperature
- Sand = Medium/High Temperature + Low Rain
- Grass = Medium Temperature + Medium/High Rain
- Desert = Low rain + High Temperature
- Trees = High rain or Low/Medium Temperature

![key](https://user-images.githubusercontent.com/64329402/166106189-fd575240-a007-446d-bf62-5d737c0704b2.jpg)

## Images

![preview](https://user-images.githubusercontent.com/64329402/164213463-98df838e-cf8a-42cc-b278-5f6cb4616472.jpg)
![herrain2](https://user-images.githubusercontent.com/64329402/166105439-a5d850de-53bc-4843-a4f8-dc423a04b0fe.jpg)

## Inputs
Pressing the 'r' key generates a new set of noise values, creating a new map.

## Libraries
- hsnoise-0.0.2 (3D Noise generator); published by Colin Hill, 2011
- gloss-1.13.2.2 (Graphics and simulations); published by Ben Lippmeier, 2022
