import yaml
import matplotlib.pyplot as plt
import numpy as np
# Ouvrir le fichier YAML et le charger
with open('data.yaml', 'r') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# Extraire les données de testname et de duration dans des listes
testnames = [item['testname'] for item in data]
durations = [item['duration'] for item in data]
# _durations = [item['duration'] for item in data if item['testname'] == 'TestsDePerformance']

# durations = [item['duration'] for item in data]

# # Calcul de la moyenne et de l'écart type
# moyenne = np.mean(durations)
# ecart_type = np.std(durations)

# # Création d'un histogramme des données
# plt.hist(durations, bins=20, color="blue", alpha=0.8)
# plt.title("Histogramme des Durées")
# plt.xlabel("Durées")
# plt.ylabel("Fréquences")

# # Affichage de la moyenne et de l'écart type sur le graphique
# plt.axvline(moyenne, color='red', linestyle='dashed', linewidth=2, label=f'Moyenne: {moyenne:.2f}')
# plt.axvline(moyenne + ecart_type, color='green', linestyle='dashed', linewidth=2, label=f'Écart Type: {ecart_type:.2f}')
# plt.axvline(moyenne - ecart_type, color='green', linestyle='dashed', linewidth=2)

# # Légende
# plt.legend()

# Affichage du graphique
#plt.show()
# Créer un graphique à barres
plt.figure(figsize=(10, 6))
plt.bar(testnames, durations, color='skyblue')
plt.xlabel('Test Name')
plt.ylabel('Duration')
plt.title('Duration of Tests')
plt.xticks(rotation=45, ha='right')  # Pour afficher les noms des tests en rotation
plt.tight_layout()

# Afficher le graphique
plt.show()
