<template>
  <div v-if="pokemon" class="modal-overlay" @click="$emit('close-modal')">
    <div class="modal" @click.stop>
      <!-- <img v-if="pokemon.images.small" class="check" :src="pokemon.images.small" alt="" /> -->
      <h6>{{ name }}</h6>
      <div class="data card-body">

        <v-container>
          <v-row>
            <v-col cols="12">
              <v-card class="elevation-2">
                <v-card-title>Choisir une Collection et un Utilisateur</v-card-title>

                <v-card-text>
                  <v-row>
                    <v-col cols="12" sm="6">
                      <v-select
                        v-model="selectedCollection"
                        :items="collections"
                        label="Collection"
                      ></v-select>
                    </v-col>
                  </v-row>

                  <v-row>
                    <v-col cols="12" sm="6">
                      <v-select
                        v-model="selectedUser"
                        :items="users"
                        label="Utilisateur"
                      ></v-select>
                    </v-col>
                  </v-row>
                </v-card-text>

                <v-card-actions>
                  <v-btn color="primary" @click="submitForm">Valider</v-btn>
                </v-card-actions>
              </v-card>
            </v-col>
          </v-row>
        </v-container>
      </div>
    </div>
    <div>
      <div class="close" @click="$emit('close-modal')">
        <img class="close-img" src="../assets/close_icon.png" alt="" />
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: "CardDetail",

  props: {
    pokemon: {},
    name: String,
  },
  data() {
    return {
      selectedCollection: null,
      selectedUser: null,
      collections: ['Legendes', 'Cartes Pokémon rares', 'Les combattants d\'élite'], 
      users: ['KissBaby', 'NourSexy', 'Léonidas'], 
    };
  },
  methods:{
    submitForm() {
      console.log('Collection sélectionnée :', this.selectedCollection);
      console.log('Utilisateur sélectionné :', this.selectedUser);
    },
  }
};
</script>

<style lang="scss" scoped>
.modal-overlay {
  z-index: 1040;
  position: fixed;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  display: flex;
  justify-content: center;
  background-color: #000000da;
  
}

.modal {
  text-align: center;
  background-color: white;
  height: 650px;
  width: 450px;
  margin-top: 2%;
  padding: 60px 0;
  border-radius: 20px;
}
.close {
  margin: 10% 0 0 16px;
  cursor: pointer;
}

.close-img {
  width: 25px;
}

.check {
  width: 100px;
}

h6 {
  font-weight: 500;
  font-size: 28px;
  margin: 20px 0;
}

p {
  font-size: 16px;
  margin: 20px 0;
}

button {
  background-color: #ac003e;
  width: 150px;
  height: 40px;
  color: white;
  font-size: 14px;
  border-radius: 16px;
  margin-top: 50px;
}

.data {
  display: flex;
  justify-content: flex-start;
  align-items: center;
  flex-direction: column;
  width: 100%;
  margin-bottom: 40px;
}
.property {
  width: 90%;
  max-width: 400px;
  border-bottom: 1px solid #ccc;
  margin-bottom: 10px;
}
.left {
  float: left;
}
.right {
  float: right;
}
h3 {
  width: 90%;
  max-width: 400px;
  border-bottom: 1px solid #ccc;
}

.types,
.abilities {
  display: flex;
  justify-content: flex-start;
  flex-wrap: wrap;
  width: 90%;
  max-width: 400px;
}
.type {
  // color: rgb(17, 67, 182);
  margin: 0 0 10px 0;
  padding: 5px 10px;
  font-weight: bold;
  font-size: 1rem;
  letter-spacing: 2px;
  text-transform: capitalize;
  span {
    color: #ffffff !important;
    padding: 10px 14px;
    border-radius: 29px;
  }
}
.ability {
  color: rgb(55, 125, 190);
  margin: 10px 15px 10px 0;
  border-radius: 20px;
  padding: 5px 10px;
  font-weight: bold;
  font-size: 1rem;
  letter-spacing: 2px;
  text-transform: capitalize;
  word-wrap: none;
  word-break: keep-all;
  background-color: #ffffff;
  border: 3px solid;
}

i {
  font-size: 2rem;
  color: #efefef;
}
.destaq {
  font-weight: bold;
}
.inactive {
  display: none;
}
</style>