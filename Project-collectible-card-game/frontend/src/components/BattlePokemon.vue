<template>
    <div v-if="enemy" style="padding-top: 300px;" class="battle">
      <div class="enemy">
        <img :src="enemy.image" :alt="enemy.name" />
        <div class="hpBarWrapper">
          <span>{{ enemy.name }}</span>
          <span>{{ enemyHp }}</span>
          <div
            :class="enemyHpClass"
            class="hpBar"
            :style="{ width: `${enemyHp}%` }"
          ></div>
        </div>
      </div>
      <div class="player">
        <img :src="chalenger.image" :alt="chalenger.name" />
        <div class="hpBarWrapper">
          <span>{{ chalenger.name }}</span>
          <span>{{ hp }}</span>
          <div :class="hpClass" class="hpBar" :style="{ width: `${hp}%` }"></div>
        </div>
      </div>
      <div class="options">
        <template v-for="n in 3">
          <v-btn
            v-if="chalenger.attacks[n]"
            color="primary"
            :disabled="gameOver"
            :key="n"
            @click="attack"
            >{{ chalenger.attacks[n].name }}</v-btn
          >
          <v-btn v-else :disabled="gameOver" color="primary" :key="n+2" @click="attack">
            Attack
          </v-btn>
        </template>
        <v-btn color="green" :disabled="gameOver" @click="heal">Heal</v-btn>
      </div>
      <div class="log">
        <div v-for="([first, second], i) in splitLogs" :key="i">
          <span>{{ first }}</span>
          <span>{{ second }}</span>
        </div>
      </div>
      <v-alert
          v-if="won && gameOver"
          class="mt-4"
          type="success"
          dismissible
          closable
        >
          Vous avez gagnez !
        </v-alert>
        <v-alert
          v-if="!won && gameOver"
          class="mt-4"
          type="error"
          dismissible
          closable
        >
          Vous avez perdu !!!!
        </v-alert>
    </div>
  </template>
  
  <script>
  /* eslint-disable */
  import { splitEvery } from "ramda";
  import { mapGetters } from "vuex";
  import {Transfer} from  "../../API/Collection";
  
  export default {
    name: "BattlePokemon",
    data() {
      return {
        hp: null, 
        enemyHp: null,
        log: [],
        gameOver: false,
        won: false,
      };
    },
    computed: {
      ...mapGetters(["enemy", "chalenger"]),
      hpClass() {
        if (this.hp > 90) return "green";
        if (this.hp > 80) return "info";
        if (this.hp > 50) return "warning";
        return "red";
      },
      enemyHpClass() {
        if (this.enemyHp > 90) return "green";
        if (this.enemyHp > 80) return "info";
        if (this.enemyHp > 50) return "warning";
        return "red";
      },
      splitLogs() {
        return splitEvery(2, this.log);
      }
    },
    mounted(){
      
    },
    methods: {
      attack() {
        const dmg = this.randomDamage();
        this.addActionToLog(
          `${this.chalenger.name} dealt ${dmg} dmg to ${this.enemy.name}`
        );
        this.enemyHp -= dmg;
        if (this.enemyHp <= 0) return this.resetData();
  
        this.enemyMove();
      },
      heal() {
        if (this.hp + 15 >= 100) {
          this.addActionToLog(`${this.chalenger.name} healed to 100`);
          this.hp = 100;
        } else {
          this.addActionToLog(`${this.chalenger.name} healed to ${this.hp + 15}`);
          this.hp += 15;
        }
  
        this.enemyMove();
        if (this.hp <= 0) this.resetData();
      },
      enemyMove() {
        if (this.enemyHp > 90) return this.enemyAttack();
  
        if (this.enemyHp > 70) {
          if (Math.random() > 0.8) return this.enemyHeal();
          return this.enemyAttack();
        }
        if (this.enemyHp > 40) {
          if (Math.random() > 0.7) return this.enemyHeal();
          return this.enemyAttack();
        }
  
        if (Math.random() > 0.5) return this.enemyHeal();
        return this.enemyAttack();
      },
  
      enemyAttack() {
        const dmg = this.randomDamage();
        this.addActionToLog(
          `${this.enemy.name} dealt ${dmg} dmg to ${this.chalenger.name}`
        );
        this.hp -= dmg;
        if (this.hp < 0) this.resetData();
      },
      enemyHeal() {
        if (this.enemyHp + 15 >= 100) {
          this.addActionToLog(`${this.enemy.name} healed to 100`);
          this.enemyHp = 100;
        } else {
          this.addActionToLog(
            `${this.enemy.name} healed to ${this.enemyHp + 15}`
          );
          this.enemyHp += 15;
        }
      },
      randomDamage() {
        return 10 + Math.round(Math.random() * 15);
      },
      resetData() {
        if (this.hp <= 0) {
          this.addActionToLog("You Lost!"); 
          this.gameOver = true;
          //Transfer(this.enemy.Acount, this.chalenger.Acount, `https://api.pokemontcg.io/v2/cards/${this.enemy.id}`);
        }
        if (this.enemyHp <= 0) {
          this.addActionToLog("You Won!");
          this.gameOver = true;
          this.won = true;
          //Transfer(this.chalenger.Acount, this.enemy.Acount, `https://api.pokemontcg.io/v2/cards/${this.chalenger.id}`);
        }
      },
      randomPokemon() {
        return this.allPokemon[
          Math.floor(Math.random() * this.allPokemon.length)
        ];
      },
      addActionToLog(info) {
        this.log.unshift(info);
       }
    },
    watch:{
      enemy(val){
        if(val)
          this.enemyHp = val;
      },
      chalenger(val){
        if(val)
          this.hp = val;
      }
    }
  };
  </script>
  
  <style scoped>
  .battle {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    margin: 0 auto;
    max-width: 350px;
  }
  
  .enemy,
  .player {
    width: 100%;
    max-width: 350px;
    margin: 0 auto;
  }
  .enemy img,
  .player img {
    width: 150px;
    height: 150px;
  }
  
  .hpBarWrapper {
    position: relative;
    display: flex;
    justify-content: space-between;
    background: rgba(43, 43, 43, 0.2);
    border: 2px solid rgb(43, 43, 43);
  }
  .hpBarWrapper span {
    z-index: 2;
    padding: 0 10px;
  }
  
  .hpBar {
    position: absolute;
    height: 25px;
    z-index: 0;
    max-width: 346px;
  }
  
  .options {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
  }
  .options button {
    width: 48%;
    height: 35px;
    line-height: 35px;
    margin: 4px 2px;
  }
  
  .log {
    overflow-y: scroll;
    margin-top: 20px;
    height: 165px;
    background: rgb(82, 82, 82);
    color: white;
  }
  .log div {
    display: flex;
    flex-direction: column;
    border-bottom: 1px solid rgb(32, 32, 32);
    margin-bottom: 10px;
  }
  </style>
  