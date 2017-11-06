import {Socket} from "phoenix"

window.app1 = new Vue({
  el: "#app1",
  data: {
    damage_types: []
  }

});

Vue.component('damage_type', {
  props: ['damage_type'],
  template: '<tr><td><input v-model="damage_type.name" ref="input" v-bind:id="damage_type.id" v-bind:name="damage_type.name" v-on:input="updateName($event.target.id, $event.target.value)"></input></td></tr>',
  methods: {
    updateName: _.debounce(function(id, name) {
      chan.push("update_name", {id: id, name: name})
      .receive("ok", () => console.log("Name Updated") )
    }, 500)
  }
})

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();
var chan = socket.channel("admin", {character: characterID});

chan.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

chan.on("damage_types", function(data){
  console.log(data.damage_types);
  app1.damage_types = data.damage_types;
});