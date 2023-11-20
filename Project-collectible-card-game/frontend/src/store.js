// store.js
import { createStore } from 'vuex';
import { getAllNfts, getnfts } from './../API/BlockChainContact';
import allSetsData from './../DataOffChain/allSetsData';
import router from './router';

const store = createStore({
  state: {
    isConnected: false,
    myNFTs: [],
    collections: allSetsData,
    userAddress : undefined,
    selectedCollectionCards : [],
    enemy : undefined,
    chalenger : undefined,
  },
  mutations: {
    setConnexionStatus: (state, isConnected) => (state.isConnected = isConnected),
    setMyNFTs: (state, nfts) => (state.myNFTs = nfts),
    setCollections: (state, collections) => (state.collections = collections),
    setSelectedCollectionCards : (state, collectionCard) => (state.selectedCollectionCards = collectionCard),
    setUserAddress : (state, address) => (state.userAddress = address),
    setEnemy : (state ,currentEnemy ) =>  (state.enemy = currentEnemy),
    setChalenger : (state , currentChalenger) => (state.chalenger = currentChalenger),
  },
  actions: {

    fetchMyNFTs: async ({commit, getters}) =>{
        // axios
        // .get(
        //   "https://api.pokemontcg.io/v2/cards?pageSize=100"
        // )
        // .then((response) => {
        //   // Le contenu du fichier JSON est dans response.data
        //   commit("setMyNFTs", response.data.data);
        // })
        // .catch((error) => {
        //   console.error("Erreur lors du chargement du fichier JSON", error);
        // }); 
         try {
        const nftsData = await getnfts(getters.userAddress);
        console.log(nftsData);
        commit("setMyNFTs", nftsData);
        // if(state.myNFTs.length == 5)
        // {
        //   try {
        //           const usersData = await fetch(accounts);
        //           const users = await usersData.json();
        //           const userToModify = users.find(user => user.userAddress === this.userAddress && user.hadBooster == false);
        //           console.log(userToModify.BoosterNumber)
        //           if (userToModify) {
        //             userToModify.BoosterNumber = 3;
        //             userToModify.hadBooster = true
        //             await fetch(accounts, {
        //               method: 'POST',
        //               body: JSON.stringify(users),
        //               headers: { 'Content-Type': 'application/json' },
        //             });
        //           if(userToModify.BoosterNumber > 0)
        //           {
        //             this.number = userToModify.BoosterNumber;
        //                this.afficherBouton = true ;
        //                console.log(this.afficherBouton);

        //           }
        //             // Mettez à jour l'affichage ou effectuez d'autres actions nécessaires
        //           }
        //         } catch (error) {
        //           console.error('Error fetching NFTs:', error);
        //         }
        // }
      } catch (error) {
        console.error('Error fetching NFTs:', error);
      }
    }, 

    fetchCollectionCards: async ({commit}, collectionId) =>{
      // for (let i = 1; i < collectionTotalCard-1; i++) {
      //   axios
      //  .get(
      //    `https://api.pokemontcg.io/v2/cards/${collectionId}-${i}`
      //  )
      //  .then((response) => {
      //   commit("setSelectedCollectionCards", response.data)
      //   console.log( response.data);
         
      //  })
      //  .catch((error) => {
      //    console.error("Erreur lors du chargement du fichier JSON", error);
      //  });
      // }
      console.log("Sexy",collectionId);
      const nftsData = await getAllNfts(collectionId);
      commit("setSelectedCollectionCards", nftsData);
      console.log( nftsData);
      router.push('/collection-cards');
    },
  },
  getters: {
    isConnected: state => state.isConnected,
    myNFTs: state => state.myNFTs,
    collections: state => state.collections,
    selectedCollectionCards : state => state.selectedCollectionCards,
    pokemonLoading: state => state.myNFTs.length === 0,
    collectionsLoading: state => state.collections.length === 0,
    collectionCardLoading: state => state.selectedCollectionCards.length === 0,
    userAddress: state => state.userAddress,
    enemy : state => state.enemy,
    chalenger : state => {
      let strongestHp = 0;
      let chalengerHp = undefined;
      for (let i = 0; i < state.selectedCollectionCards.length; i++) {
        if (state.selectedCollectionCards[i].hp > strongestHp){
          strongestHp = state.selectedCollectionCards[i].hp;
          chalengerHp = state.selectedCollectionCards[i];
        }
      }
      return chalengerHp;
    },

}
})

export default store
