const Web3 = require('web3');
const web3 = new Web3('**');
const contract = require("../../contracts/artifacts/src/Collection.sol/Collection.json");
const contractABI = contract.abi; // Your contract's ABI
const nftContract = new web3.eth.Contract(contractABI, '**');
const fs = require('fs');
const axios = require('axios'); // Import the 'axios' module
const path = "../DataOffChain/allSetsData.json";

async function createAllCollections() {
  const url = 'https://api.pokemontcg.io/v2/sets';
  try {
    let count = 0;
    const response = await axios.get(url);
    const data = response.data;
    const sets = data.data;
    const allSetsData = [];
    for (const set of sets) {
      count ++;
      const setId = set.id;
      const setName = set.name;
      const setImage = set.images.symbol;
      const cardCount = set.total;
      //await createCollection(setName, setImage);
      const setData = { setId ,setName, setImage ,cardCount};
      allSetsData.push(setData);
      if(count >= 5)
      {
        break;
      }
    }
    const dataToSave = JSON.stringify(allSetsData, null, 2);
    fs.writeFileSync(path, dataToSave);

    console.log('Data for all sets saved to allSetsData.json successfully.');
  } catch (error) {
    console.error('An error occurred:', error);
  }
}

async function createCollection(_name,symbol) {
  try {
    const apiUrl = `https://api.pokemontcg.io/v2/cards?q=id:base1-4`;
    const _result = await nftContract.methods.createNewCard(0,apiUrl,"0xb5b9e3C75e451228A23aD982627aE6C1Cc342aDD").call();
    console.log("Card 1")
    console.log("Transaction hash:", result.transactionHash);
    console.log("Collection créée avec succès !");
  } catch (error) {
    console.error('Erreur lors de la création de la collection :', error.message);
    // Affichez plus d'informations sur l'erreur
  }
}

createAllCollections();