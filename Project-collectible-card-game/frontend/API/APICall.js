const express = require('express');
const axios = require('axios');
const app = express();
const PORT = process.env.PORT || 3000;
const fs = require('fs');
const cors = require('cors'); 
const bodyParser = require('body-parser');
const port = 3000; // Port sur lequel l'API écoutera
app.use(express.json());
const path = "../DataOffChain/accounts.json";
app.use(cors());
app.get('/cards/:set/:id', async (req, res) => {
  try {
    const set = req.params.set;
    const cardId = req.params.id;
    const apiUrl = `https://api.pokemontcg.io/v2/cards/${set}-${cardId}`;
    const response = await axios.get(apiUrl);
    const cardData = response.data.data;
    const importantData = {
      
      name: cardData.name,
      image: cardData.images.large,
      type: cardData.types,
      id: cardData.id,
      level: cardData.level,
      hp: cardData.hp,
      attacks: cardData.attacks,
      weaknesses: cardData.weaknesses,
      retreatCost: cardData.retreatCost,
      set: cardData.set.id,
      // Add other important properties here
    };

    res.json(importantData);
  } catch (error) {
    res.status(500).json({ error: 'An error occurred' });
  }
});


app.get('/accounts', (req, res) => {
  fs.readFile(path, 'utf-8', (err, data) => {
    if (err) {
      console.error(err);
      res.status(500).send('Erreur de lecture du fichier');
      return;
    }

    const accounts = JSON.parse(data);
    res.json(accounts);
  });
});

app.get('/sets', (req, res) => {
  fs.readFile('../DataOffChain/allSetsData.json', 'utf-8', (err, data) => {
    if (err) {
      console.error(err);
      res.status(500).send('Erreur de lecture du fichier');
      return;
    }
    const accounts = JSON.parse(data);
    res.json(accounts);
  });
});

const users = [];

app.use(bodyParser.json());
app.post('/accounts', (req, res) => {
  const newUser = req.body;
  users.push(newUser);
  saveUsersToFile(users);
  res.json(newUser);
  console.log(newUser);
});

function saveUsersToFile(usersData) {
  const jsonUsers = JSON.stringify(usersData, null, 2);

  fs.writeFile(path, jsonUsers, 'utf8', (err) => {
    if (err) {
      console.error('Erreur lors de l\'enregistrement des utilisateurs dans le fichier :', err);
    } else {
      console.log('Utilisateurs enregistrés avec succès');
    }
  });
}

app.listen(port, () => {
  console.log(`Serveur Express écoutant sur le port ${port}`);
});

