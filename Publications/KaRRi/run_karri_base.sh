#!/bin/bash


# Skript zum Ausführen von KaRRi auf Compute-Servern.
# Verwendung: screen exclusive bash run_karri_base.sh <source-dir> <instance-name> <output-base-dir> [timeout]
#	- <source-dir> : absoluter Pfad zum untersten Ordner deines Repository, also wahrscheinlich sowas wie /home/kuchenbecker/karri/
# 	- <instance-name> : entweder Berlin-1pct oder Berlin-10pct
#	- <output-base-dir> : absoluter Pfad zu frei gewähltem Output-Ordner, z.B. /home/kuchenbecker/Outputs/ (Ordner muss bereits vor Aufruf existieren)
#	- <radius> : Radius, in dem PDLocs gesucht werden (in Sekunden). Üblicherweise 300.
#	- [timeout]: optionales timeout für jeden Run in Sekunden

# Zur Erinnerung, was die Präfixe bedeuten:
# 	screen : Führe Befehl in einem screen, also extra erzeugtem Terminal aus (damit man Terminal schließen kann und später wieder mit screen -r resumen kann)
# 	exclusive : Lege für die Dauer des Befehls ein Reservierungs-Token auf die Maschine

# Lies Eingabeparameter
karriSourceDir=$1
instanceName=$2
outputBaseDir=$3
radius=$4
timeout=$5

# Farben für Konsolen-Output
green='\033[0;32'
no_color='\033[0m'

# Prüfe, ob Output Directory existiert
if ! [ -d "$outputBaseDir" ]; then
	echo "Output directory ${outputBaseDir} does not exist."
	exit 1
fi

# Setze Timeout auf 90 Minuten, falls nicht anderweitig angegeben
if [ -z ${timeout} ]; then 
	timeout="90m"
fi
echo "Using timeout of ${timeout}."


# Hard-gecodete Orte für Inputs
inputDir=/global_data/laupichler/KaRRi/Inputs
vehGraph=$inputDir/Graphs/${instanceName}_pedestrian_veh.gr.bin
psgGraph=$inputDir/Graphs/${instanceName}_pedestrian_psg.gr.bin
vehicles=$inputDir/Vehicles/${instanceName}_pedestrian.csv
requests=$inputDir/Requests/${instanceName}_pedestrian.csv
vehCh=$inputDir/CHs/${instanceName}_pedestrian_veh_time.ch.bin
psgCh=$inputDir/CHs/${instanceName}_pedestrian_psg_time.ch.bin
sepDecomp=$inputDir/SepDecomps/${instanceName}_nd30.sep.bin

# Defines for different Filter Strategies
heuristics=("ALL" "MAX_RAND" "CH_ABS" "CH_REL" "PARETO_SIMPLE" "PARETO_DIR")

# Erzeuge konkretes Output-Directory, dessen Name aus instanceName + radius + aktuellem timestamp besteht.
currentTime=$(date "+%Y.%m.%d-%H:%M")
karriOutputDir=$outputBaseDir/LOUD/${instanceName}_${radius}_${currentTime}
mkdir -p $karriOutputDir

# Baue KaRRi nach $karriSourceDir/Build/Release; gib explizit hard-gecodeten Ort der dependencies an.
# Konfiguriere KaRRi, sodass individual last-stop BCH Searches und kein SIMD-Parallelismus verwendet werden.

for strat in ${heuristics[@]}
do

karriBinaryDir=$karriSourceDir/Build/Release
dependencyInstallDir=/global_data/laupichler/KaRRi/install
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH="${dependencyInstallDir}"\
	-DKARRI_ELLIPTIC_BCH_USE_SIMD=ON -DKARRI_ELLIPTIC_BCH_LOG_K=4 \
	-DKARRI_PALS_STRATEGY=COL -DKARRI_PALS_USE_SIMD=ON -DKARRI_PALS_LOG_K=3 \
	-DKARRI_DALS_STRATEGY=COL -DKARRI_DALS_USE_SIMD=ON -DKARRI_DALS_LOG_K=3 \
	-DKARRI_PD_DISTANCES_USE_SIMD=ON -DKARRI_PD_DISTANCES_LOG_K=5 \
	-DKARRI_PSG_COST_SCALE=1 \
	-DKARRI_VEH_COST_SCALE=1 \
	-DKARRI_FILTER_STRATEGY=CH_ABS \
	-S $karriSourceDir -B ${karriBinaryDir}_$strat
cmake --build ${karriBinaryDir}_${strat} --target karri -j 16

done

echo -e "${green}Starting Test Runs${no_color}"
# Lasse KaRRi für jede Strategie 5 Mal laufen (nur relevant für Laufzeitmessungen, für Qualität reicht ein Run)

echo -e "${green}Starting Runs with Filter Strategy ALL${no_color}"
# 1: All PD Locs
for i in {1..5}
do

# Run pedestrian/KaRRi, radius 300, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_ALL/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/ALL_${run_id}

done

echo -e "${green}Done with Runs with Filter Strategy ALL${no_color}"
echo -e "${green}Starting Runs with Filter Strategy MAX_RAND${no_color}"

# 2: Maximum of k randomly chosen PD Locs, k in {1,2,3,4,5,10,15,20,25}
kValues=(1 2 3 4 5 10 15 20 25)
for k in ${kValues[@]}
do

for i in {1..5}
do

# Run pedestrian/KaRRi, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_MAX_RAND/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/MAX_RAND_$k_${run_id} -max-num-d $k

done

done

echo -e "${green}Done with Runs with Filter Strategy MAX_RAND${no_color}"
echo -e "${green}Starting Runs with Filter Strategy CH_ABS${no_color}"

# 2: Maximum of k PD Locs, picking those with the highest rank in the CH, k in {1,2,3,4,5,10,15,20,25}
kValues=(1 2 3 4 5 10 15 20 25)
for k in ${kValues[@]}
do

for i in {1..5}
do

# Run pedestrian/KaRRi, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_CH_ABS/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/CH_ABS_$k_${run_id} -max-num-d $k

done

done

echo -e "${green}Done with Runs with Filter Strategy CH_ABS${no_color}"
echo -e "${green}Starting Runs with Filter Strategy CH_REL${no_color}"

# 2: Pick all PD Locs whose rank is with in k percent of the rank of the PD Loc with the highest rank in the radius, k in {1,2,3,4,5,6,8,10,12,15,20}
kValues=(1 2 3 4 5 6 8 10 12 15 20)
for k in ${kValues[@]}
do

for i in {1..5}
do

# Run pedestrian/KaRRi, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_CH_REL/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/CH_REL_$k_${run_id} -max-num-d $k

done

done

echo -e "${green}Done with Runs with Filter Strategy CH_REL${no_color}"
echo -e "${green}Starting Runs with Filter Strategy PARETO_SIMPLE${no_color}"

# 2: Pick all PD Locs that are not dominated by more than k PD Locs both in terms of CH rank and in terms of distance from the origin, k in {1,2,3,4,5,6,7}
kValues=(1 2 3 4 5 6 7)
for k in ${kValues[@]}
do

for i in {1..5}
do

# Run pedestrian/KaRRi, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_PARETO_SIMPLE/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/PARETO_SIMPLE_$k_${run_id} -max-num-d $k

done

done

echo -e "${green}Done with Runs with Filter Strategy PARETO_SIMPLE${no_color}"
echo -e "${green}Starting Runs with Filter Strategy PARETO_DIR${no_color}"

# 2: Pick all PD Locs that are not dominated by more than k PD Locs both in terms of CH rank and in terms of distance from the origin and make sure that in every octant there are at least k PD Locs (if possible), k in {1,2,3,4,5,6,7}
kValues=(1 2 3 4 5 6 7)
for k in ${kValues[@]}
do

for i in {1..5}
do

# Run pedestrian/KaRRi, wait time 300
# ID, um zwischen 5 runs zu unterscheiden
run_id=KaRRi_run$i
timeout $timeout taskset 0x1 ${karriBinaryDir}_PARETO_DIR/Launchers/karri -w 300 -p-radius $radius -d-radius $radius -veh-g $vehGraph -psg-g $psgGraph -v $vehicles -r $requests -veh-h $vehCh -psg-h $psgCh -o $karriOutputDir/PARETO_DIR_$k_${run_id} -max-num-d $k

done

done

echo -e "${green}Done with Runs with Filter Strategy PARETO_DIR${no_color}"

# Bemerkung:
#
# Das Präfix "timeout $timeout" bricht den Prozess ab, sobald das Timeout erreicht wird, welches in der Variable $timeout angegeben ist.
# $timeout ist by default in Sekunden angegeben, aber auch andere Einheiten sind möglich, z.B. "90m" sind 90 Minuten. 
# Siehe auch man timeout.
#
# Das Präfix "taskset 0x1" pinnt den Prozess auf den ersten Prozessor. Dann kann das Betriebssystem 
# also nicht frei den Prozess zwischen den Prozessoren bewegen, sondern der Prozess wird auf einem 
# Prozessor bleiben. Das hilft dabei, saubere Zeitmessungen zu kriegen. 

