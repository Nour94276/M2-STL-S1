import { createRouter, createWebHistory } from 'vue-router';

// Import your components
import HomePage from './components/HomePage.vue';
import AvaillablePokemonCards from './components/AvaillablePokemonCards.vue';
import CollectionCards from './components/CollectionCards';
import Collections from './components/Collections';
import AboutUs from './components/AboutUs.vue';
import BattlePokemon from './components/BattlePokemon.vue';

const routes = [
  { path: "/", component: HomePage },
  { path: "/my-NFTs", component: AvaillablePokemonCards },
  { path: "/collections", component: Collections },
  { path: "/collection-cards", component: CollectionCards },
  { path: "/battle-pokemon", component: BattlePokemon },
  { path: "/about", component: AboutUs },
];

const router = createRouter({
  history: createWebHistory(),
  routes,
});

export default router;
