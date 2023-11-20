 /* eslint-disable */
const { ethers, Wallet } = require('ethers');
// Connect to Ethereum network
const provider = new ethers.providers.JsonRpcProvider("**");
const wallet = new Wallet("**", provider);
const contract = require("../../contracts/artifacts/src/Collection.sol/Collection.json");
const contractABI = contract.abi; // Your contract's ABI
const nftContract = new ethers.Contract("**", contractABI, wallet);
const apiUrlSets = 'http://localhost:3000/sets';
const apiUrlAccount = 'http://localhost:3000/accounts';

 async function createNFTRandom(UserAdress){
     const { randomSetId, maxNumber } = getRandomSet();
     const __randomNum = getRandomUniqueNumber(minNumber, maxNumber);
     console.log(randomSetId,maxNumber);
      createNFT(__randomNum,randomSetId,UserAdress);
   console.log(__randomNum,randomSetId);
}
async function createNFT(cardId,set,UserAdress) {
     const apiUrl = `https://api.pokemontcg.io/v2/cards?q=id:${set}-${cardId}`
     console.log(apiUrl);
     const tx = await nftContract.mint(apiUrl,UserAdress);
     await tx.wait();
     console.log('NFT created:',cardId );
 }
const minNumber = 1;
const usedNumbers = [];
function getRandomUniqueNumber(min, max) {
  let randomNum;
  do {
    randomNum = Math.floor(Math.random() * (max - min + 1)) + min;
  } while (usedNumbers.includes(randomNum));

  usedNumbers.push(randomNum);
  return randomNum;
}

async function getRandomSet() {
     try {
       const response = await fetch(apiUrlSets);
       const data = await response.json();
       const randomIndex = getRandomInt(5);
       return data[randomIndex];
     } catch (error) {
       console.error('Une erreur s\'est produite lors de la récupération des données :', error);
       return null;
     }
   }
   
function getRandomInt(max) {
  return Math.floor(Math.random() * (max + 1));
}


async function getUserData(apiUrlAccount) {
  return fetch(apiUrlAccount)
    .then(response => {
      if (!response.ok) {
        throw new Error(`HTTP error! Status: ${response.status}`);
      }
      return response.json();
    })
    .then(data => {
      return data;
    })
    .catch(error => {
      console.error('An error occurred:', error);
      throw error;
    });
}


function isUserExist(userAddressToCheck, userData) {
  for (let i = 0; i < userData.length; i++) {
    if (userData[i].userAddress === userAddressToCheck) {
     return true ;
    }
    return false;
  }
 
}


export async function Transfer(from , to , URI)
{
     const tx = await nftContract.transferCard(from,to,URI);
     await tx.wait();
     console.log('NFT Tranfer:',URI);
}
 export async function  CheckExistenceUserAccount(userAddress) {

    const result = await getUserData(apiUrlAccount);
    const boolresult = isUserExist(userAddress ,result);
    const totalDataCount = result.length + 1;
    const userNameToAdd = `User${totalDataCount}`;
    const newAccount = {
      userAddress,
      userName: userNameToAdd,
      haveBooster : false,
      BoosterNumber : 0
    };
    if(!boolresult)
    {
      addUser(newAccount);
      createNFTRandom(userAddress);
      console.log("createNFTRandom",userAddress);
    }
}

function addUser(newUser) {
  return fetch(apiUrlAccount, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(newUser),
  })
    .then(response => response.json())
    .catch(error => {
      console.error('Une erreur s\'est produite lors de l\'ajout de l\'utilisateur :', error);
      throw error;
    });
}