<template>
    <v-main class="pt-10">
      <header class="header">
      <h1 >
        <img
          class="center"
          :src="'https://archives.bulbagarden.net/media/upload/4/4b/Pok%C3%A9dex_logo.png'"
          style="width: 400px"
        />
      </h1>
      <v-spacer></v-spacer>
      <v-col class="center">
        <v-text-field width="100px" placeholder="Looking for a pokemon! Let me help you, please enter a name. " v-model="searchText">
          <template >
            <v-icon> mdi-magnify</v-icon>
          </template>
        </v-text-field>
      </v-col>
      
    </header>
      <loading-spinner v-if="pokemonLoading"></loading-spinner>
      <v-container v-else>
    <v-row>
      <v-col
        v-for="(card, index) in resultQuery"
        :key="index"
        cols="12"
        sm="6"
        md="4"
        lg="3"
      >
        <v-card  max-width="350px" class="elevation-2">
          <v-img
            :src="card.image" 
            alt="Card Image"
            class="white--text"
           
          >
          </v-img>
        </v-card>
      </v-col>
    </v-row>
  
  </v-container>
  
    </v-main>
</template>


<script>
import { mapGetters, mapActions } from "vuex";
import LoadingSpinner from "./LoadingSpinner";
export default {
  name: "AvaillablePokemonCards",
  components:{
    LoadingSpinner
  },
  mounted(){
    if(this.isConnected){
      this.fetchMyNFTs();
      console.log(this.myNFTs);
    }else{
      this.$router.push('/');
    }
  
  },

  computed:{
    ...mapGetters(["myNFTs", "isConnected","pokemonLoading"]),
    resultQuery() {
        if (this.searchText) {
          return this.myNFTs.filter((pokemon) => {
            this.isLoading = false; 
            return this.searchText
              .toLowerCase()
              .split(" ")
              .every((v) => pokemon.name.toLowerCase().includes(v));
          });
          
        } else {
          return this.myNFTs;
        
        }
      },

     
  },
  methods:{
     ...mapActions(["fetchMyNFTs"])
  }
 
 
};
</script>