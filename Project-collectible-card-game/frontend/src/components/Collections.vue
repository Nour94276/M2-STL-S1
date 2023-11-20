<template>

 <v-main>
  <loading-spinner v-if="collectionsLoading"></loading-spinner>
  <v-container v-else>
    <h1 class="text-center pb-4">Les Collections</h1>
    <v-row>
      <v-col
        v-for="(collection, index) in resultQuery"
        :key="index"
        cols="12"
        sm="6"
        md="4"
        lg="3"
      >
        <v-card @click="fetchCollectionCards(collection.setId)" style="border: 6px solid rgba(22, 34, 136, 0.84); border-radius: 19px;">
          <v-img :src="collection.setImage" height="200" alt="Collection Logo"></v-img>
          <v-card-title>{{ collection.setName }}</v-card-title>
          <v-card-text>
            <p>Nombre total de cartes : {{ collection.cardCount }}</p>
          </v-card-text>
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
  name:"CollectionsCard",
  components:{
    LoadingSpinner
  },
  data() {
    return { 
    };
  },
  mounted(){
    if(!this.isConnected){
      this.$router.push('/');
    }
  
  },
  computed:{
    ...mapGetters(["collections", "isConnected","collectionsLoading"]),
    resultQuery() {
        if (this.searchText) {
          return this.collections.filter((colection) => {
            this.isLoading = false; 
            return this.searchText
              .toLowerCase()
              .split(" ")
              .every((v) => colection.name.toLowerCase().includes(v));
          });
    
        } else {
          return this.collections;
        
        }
      },
  },
 methods:{
  ...mapActions(["fetchCollectionCards"]),
 }
};
</script>

