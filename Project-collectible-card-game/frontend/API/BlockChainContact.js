const Web3 = require('web3');
const web3 = new Web3('**');
const contract = require("../../contracts/artifacts/src/Collection.sol/Collection.json");
const contractABI = contract.abi; // Your contract's ABI
const nftContract = new web3.eth.Contract(contractABI, '**');
const axios = require('axios'); // Import the 'axios' module

export async function getAllNfts(set) {
  console.log("Set :", set);
  const result = await nftContract.methods.getAllNFTsWithDetails().call();
  console.log(result);
  const DataToReturn = [];
  for (let i = 0; i < result[1].length; i++) {
    const url = result['1'][i];
    console.log("URL  :",url);
    const match = url.match(new RegExp(set + "-"));
    if(match )
    {
      const tokenId = result['0'][i];
      const Url = result['1'][i];
      const Account = result['2'][i];
      const response = await axios.get(Url);
      const data = response.data.data[0];
      if (isValidNftData(data)) {
      const nftInfo = {
        tokenId: tokenId,
        Url: Url,
        Account: Account,
        name: data.name,
        image: data.images.large,
        types: data.types,
        id: data.id,
        level: data.level.value,
        hp: data.hp,
        attacks: data.attacks,
        weaknesses: data.weaknesses,
        retreatCost: data.retreatCost,
        set: data.set.id,
      };
      console.log(nftInfo);
      DataToReturn.push(nftInfo);
    }
    }
  }
  console.log(DataToReturn);
  return DataToReturn ; 
  
}


function isValidNftData(data) {
  return (
    data && 
    data.name !== undefined && 
    data.images && 
    data.images.large !== undefined 
  );
}
export async function getnfts(User) {
  const balance = await nftContract.methods.balanceOf(User).call();
  const nfts = [];

  for (let i = 0; i < balance; i++) {
    const tokenUrl = await nftContract.methods.tokenURI(i).call();
    console.log(tokenUrl);
    const response = await axios.get(tokenUrl);
    const data = response.data.data[0];
    if (isValidNftData(data)) 
      {
        const nftInfo = {
          name: data.name,
          image: data.images.large,
          types: data.types,
          id: data.id,
          level: data.level,
          hp: data.hp,
          attacks: data.attacks,
          weaknesses: data.weaknesses,
          retreatCost: data.retreatCost,
          set: data.set,
        };
    
        nfts.push(nftInfo);
    }
  }
  return nfts; 
}


  
  
  
  
  