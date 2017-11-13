import {Socket} from "phoenix"
import ShoppingList from './vue/shopping_list.vue'
import Type from './vue/damage_types/type.vue'

window.shopping_list = new Vue({
  el: '#shopping-list',
  template: '<ShoppingList/>',
  components: { ShoppingList }
})

window.damage_types = new Vue({
  el: "#app",
  template: '#damage-types',
  data: {
    damage_types: {}
  },
  components: { Type }
});


var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();
var chan = socket.channel("admin", {character: characterID});

chan.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

chan.on("damage_types", function(data){
  console.log(data.damage_types);
  damage_types.damage_types = data.damage_types;
});

