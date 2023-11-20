<template>
  <div id="app">
    <v-app>
      <v-app-bar app color="primary">
        <v-app-bar-nav-icon ></v-app-bar-nav-icon>
        <v-toolbar-title>Pokebattle</v-toolbar-title>
        <v-spacer></v-spacer>
          
          <v-btn text v-if="isConnected" >
            <router-link to="/my-NFTs" style="color: white;text-decoration: none;"  >
              Mes NFTs
            </router-link>
          </v-btn>

          <v-btn text v-if="isConnected"  >
            <router-link to="/collections" style="color: white;text-decoration: none;"  >
              Collections
            </router-link>
          </v-btn>

          <v-btn text >
            <router-link to="/about" style="color: white;text-decoration: none;"  >
              À propos
            </router-link>
          </v-btn>
      </v-app-bar>

      <v-content>
      <v-container class="fill-height" fluid>
        <v-row align="center" justify="center">
          <v-col class="text-center">
            <router-view />
          </v-col>
        </v-row>
      </v-container>
    </v-content>

      <v-footer padless>
        <v-col class="text-center" cols="12">
          © 2023 — <strong>Pokedex</strong>
        </v-col>
      </v-footer>
    </v-app>
  </div>
</template>


<script>
import { mapGetters } from "vuex";
import { getAllNfts, getnfts } from './../API/BlockChainContact';
import accounts from './../DataOffChain/accounts';
import allSetsData from './../DataOffChain/allSetsData';
export default {
  data() {
    return {
      web3: null,
      nfts: [],
      userAddress: '',
      userBalance: 0,
      collections: allSetsData,
      collectionDetails: [],
      afficherBouton: false,
      number : 0
    };
  },
  computed:{
    ...mapGetters(["isConnected"]),
  },
  methods: {

    async fetchNFTs() {
      try {
        const nftsData = await getnfts(this.userAddress);
        this.nfts = nftsData;
        if(this.nfts.length == 5)
        {
          try {
                  const usersData = await fetch(accounts);
                  const users = await usersData.json();
                  console.log("aaaaaaaaaa")
                  const userToModify = users.find(user => user.userAddress === this.userAddress && user.hadBooster == false);
                  console.log(userToModify.BoosterNumber)
                  if (userToModify) {
                    userToModify.BoosterNumber = 3;
                    userToModify.hadBooster = true
                    await fetch(accounts, {
                      method: 'POST',
                      body: JSON.stringify(users),
                      headers: { 'Content-Type': 'application/json' },
                    });
                  if(userToModify.BoosterNumber > 0)
                  {
                    this.number = userToModify.BoosterNumber;
                       this.afficherBouton = true ;
                       console.log(this.afficherBouton);

                  }
                    // Mettez à jour l'affichage ou effectuez d'autres actions nécessaires
                  }
                } catch (error) {
                  console.error('Error fetching NFTs:', error);
                }
        }
      } catch (error) {
        console.error('Error fetching NFTs:', error);
      }
} ,
 async fetchCollectionNfts(collectionInfo) {
  const setId = collectionInfo.setId;
  const nftsData = await getAllNfts(setId);
  this.collectionDetails = nftsData;
},

  },
};
</script>
<style scoped>
.round-button {
  width: 80px;
  height: 80px;
  background-color: #3498db;
  color: #fff;
  border: none;
  border-radius: 50%;
  font-size: 24px;
  cursor: pointer;
  transition: background-color 0.3s;
}

.round-button:hover {
  background-color: #2980b9;
}
.collection-list {
  list-style: none;
  padding: 0;
  display: flex;
  flex-wrap: wrap;
}

.collection-item {
  border: 1px solid #ccc;
  padding: 10px;
  margin: 10px;
  width: 200px;
  text-align: center;
}

.collection-title {
  font-size: 1.2em;
  margin: 0;
}

.collection-name {
  font-weight: bold;
}

.collection-image {
  max-width: 100%;
  height: auto;
}

.collection-count {
  margin: 0;
}
.collection-details-cards {
  display: flex;
  flex-wrap: wrap;
  gap: 20px; /* Espace entre les cartes */
}

.collection-detail-card {
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  border-radius: 8px;
  padding: 20px;
  width: 300px; /* Largeur de chaque carte */
  box-shadow: 0 0 5px rgba(0, 0, 0, 0.2);
}

.card-content {
  text-align: left;
}

.detail-Image {
  max-width: 100%; /* Pour redimensionner l'image à l'intérieur de la carte */
}

</style>