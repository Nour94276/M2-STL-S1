<template>
  <div class="home-page">
   <v-main >
    <v-container fluid>
        <v-card height="850px" class="text-center" flat >
          <v-img
            src="https://www.esports.net/wp-content/uploads/2022/02/are-there-pokemon-nfts.jpg"
            alt="Pokémon World"
            height="600"
            width="2000"
          ></v-img>
          <v-card-title class="text-h5">Bienvenue sur notre plateforme de jeu pokemon </v-card-title>
          <v-card-text>
            <p>Pour commencer, cliquez sur le bouton ci-dessous pour récupérer vos NFts et lancer des battles</p>
          </v-card-text>
          <v-btn color="primary" @click="connectMetaMask">Se connecter à MetaMask</v-btn>
        </v-card>
    </v-container>
    <v-alert
          v-if="isConnected && showAltert"
          class="mt-4"
          type="success"
          dismissible
          closable
        >
          Connexion établie à MetaMask !
        </v-alert>
        <v-alert
          v-if="!isConnected && showAltert"
          class="mt-4"
          type="error"
          dismissible
          closable
        >
          Connexion à MetaMask requise.
        </v-alert>
  
   </v-main>
  </div>
</template>


<script>
import Web3 from 'web3';
import { CheckExistenceUserAccount } from "../../API/Collection";
export default {
  data(){
    return{
      isConnected: false,
      showAltert: false,
    };
  },

  methods: {
    async connectMetaMask() {
      if (typeof window.ethereum !== 'undefined') {
        this.web3 = new Web3(window.ethereum);
        try {
          await window.ethereum.enable();
          this.isConnected = true;
          const accounts = await this.web3.eth.getAccounts();
          console.log(accounts);
          this.userBalance = this.web3.utils.fromWei(await this.web3.eth.getBalance(accounts[0]), 'ether');
          this.showAltert = true;
          this.$store.commit("setConnexionStatus", true);
          this.$store.commit("setUserAddress",accounts[0]);
          CheckExistenceUserAccount(accounts[0]);
          this.$router.push('/my-NFTs');
        } catch (error) {
          console.error(error);
          this.showAltert = true;
        }
      } else {
        console.error('MetaMask is not installed. Please install and try again.');
        this.showAltert = true;
      }
    },
  },
};
</script>
