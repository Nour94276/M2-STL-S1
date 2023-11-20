<template>
    <v-main class="pt-7">
        <v-btn @click="goBack" style="top: 30px;" color="primary" class="ma-4">
            <v-icon>mdi-star</v-icon>
            Retour
        </v-btn>
        <v-container v-if="selectedCollectionCards">
            <v-row>
                <v-col
                    v-for="(card, index) in selectedCollectionCards"
                    :key="index"
                    cols="12"
                    sm="6"
                    md="4"
                    lg="3"
                >
                    <v-card class="elevation-2">
                    <v-img
                        :src="card.image" 
                        alt="Card Image"
                        class="white--text"
                    >
                    </v-img>
                    <v-card-text>
                        Joueur : {{card.Account}}
                    </v-card-text>
                    <v-card-actions v-if="userAddress!=card.Account">
                        <v-btn @click="startBattle(card)" color="primary"> Démarrer une battle</v-btn>
                    </v-card-actions>
                    </v-card>
                </v-col>
            </v-row>
        </v-container>
    </v-main>
</template>


<script>
import { mapGetters } from "vuex";
export default {
  name: "CollectionCards",
  mounted(){
    if(!this.isConnected){
      this.$router.push('/');
    }
  },
  data: () => {
    return {
        
    };
    
  },
  computed:  {
    ...mapGetters(["selectedCollectionCards","userAddress", "isConnected","collectionCardLoading"]),
  },
  methods:{
    goBack() {
      this.$router.push("/collections")
    },
    startBattle(enemyCard) {
      this.$store.commit("setEnemy", enemyCard);
      this.$router.push("/battle-pokemon")
    },
  },
 
};
</script>