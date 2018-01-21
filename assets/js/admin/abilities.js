import {Socket} from "phoenix"

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();

window.store = new Vuex.Store({
  state: {
    channel: socket.channel("admin:abilities", {character: characterID}),
    abilities: {},
    sortBy: "id",
    descending: false,
    page: 1,
    valid_targets: [],
    kinds: []
  },
  mutations: {
    create_trait(state, ability_id) {
      var ability_index = _.findIndex(store.state.abilities, function(ability) {
        return ability.data.id === ability_id
      })
      state.abilities[ability_index].new_trait.show = false

      var trait_name = state.abilities[ability_index].new_trait.name
      var trait_value = state.abilities[ability_index].new_trait.value

      console.log("adding " + trait_name)

      state.channel.push("create_trait", {ability_id: ability_id, name: trait_name, value: trait_value})
      .receive("ok", function(response) {
        console.log(response.id + " added")

        var trait = {
          data: {
            ability_id: ability_id,
            id: response.id,
            name: trait_name,
            value: trait_value
          },
          form: {
            ability_id: ability_id,
            id: response.id,
            name: trait_name,
            value: trait_value
          },
          valid: true
        }

        store.commit('add_created_trait', {index: ability_index, trait: trait})
      })
    },
    create_damage_type(state, ability_id) {
      var ability_index = _.findIndex(store.state.abilities, function(ability) {
        return ability.data.id === ability_id
      })
      state.abilities[ability_index].new_damage_type.show = false

      var damage_type_name = state.abilities[ability_index].new_damage_type.name
      var damage_type_potency = state.abilities[ability_index].new_damage_type.potency
      var damage_type_kind = state.abilities[ability_index].new_damage_type.kind

      console.log("adding " + damage_type_name)

      state.channel.push("create_damage_type", {ability_id: ability_id, name: damage_type_name, potency: damage_type_potency, kind: damage_type_kind})
      .receive("ok", function(response) {
        console.log(response.id + " added")

        var damage_type = {
          data: {
            ability_id: ability_id,
            id: response.id,
            name: damage_type_name,
            kind: damage_type_kind,
            potency: damage_type_potency
          },
          form: {
            ability_id: ability_id,
            id: response.id,
            name: damage_type_name,
            kind: damage_type_kind,
            potency: damage_type_potency
          },
          valid: true
        }

        store.commit('add_created_damage_type', {index: ability_index, damage_type: damage_type})
      })
    },
    add_created_trait(state, payload) {
      var ability_index = payload.index
      state.abilities[ability_index].data.traits.unshift(Object.assign({}, payload.trait))
      state.abilities[ability_index].form.traits.unshift(Object.assign({}, payload.trait))
      state.abilities[ability_index].new_trait.show = false
      state.abilities[ability_index].new_trait.name = ""
      state.abilities[ability_index].new_trait.value = ""
    },
    add_created_damage_type(state, payload) {
      var ability_index = payload.index
      state.abilities[ability_index].data.damage_types.unshift(Object.assign({}, payload.damage_type))
      state.abilities[ability_index].form.damage_types.unshift(Object.assign({}, payload.damage_type))
      state.abilities[ability_index].new_damage_type.show = false
      state.abilities[ability_index].new_damage_type.name = ""
      state.abilities[ability_index].new_damage_type.value = ""
    },
    update_ability(state, form_data) {
      console.log("updating " + form_data.name)
      var index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.id
      })
      state.abilities[index].data = Object.assign({}, form_data)
      state.channel.push("update", form_data)
      .receive("ok", function() {
        console.log(form_data.name + " Updated")
        state.abilities[index].dialog = false
      })
    },
    update_trait(state, form_data) {
      console.log("updating " + form_data.name)
      var ability_index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.ability_id
      })

      var trait_index = _.findIndex(state.abilities[ability_index].data.traits, function(item) {
        return item.form.id === form_data.id
      })

      state.channel.push("update_trait", form_data)
      .receive("ok", function() {
        console.log(form_data.name + " Updated")
        state.abilities[ability_index].data.traits[trait_index].data = Object.assign({}, form_data)
        state.abilities[ability_index].form.traits[trait_index].data = Object.assign({}, form_data)
      })
    },
    update_damage_type(state, form_data) {
      console.log("updating " + form_data.name)
      var ability_index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.ability_id
      })

      var damage_type_index = _.findIndex(state.abilities[ability_index].data.damage_types, function(item) {
        return item.form.id === form_data.id
      })

      state.channel.push("update_damage_type", form_data)
      .receive("ok", function() {
        console.log(form_data.name + " Updated")
        state.abilities[ability_index].data.damage_types[damage_type_index].data = Object.assign({}, form_data)
        state.abilities[ability_index].form.damage_types[damage_type_index].data = Object.assign({}, form_data)
      })
    },
    delete_trait(state, form_data) {
      console.log("deleting " + form_data.name)
      var ability_index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.ability_id
      })

      var trait_index = _.findIndex(state.abilities[ability_index].data.traits, function(item) {
        return item.form.id === form_data.id
      })

      state.channel.push("delete_trait", form_data.id)
      .receive("ok", function() {
        console.log(form_data.name + " deleted")
        store.commit('remove_deleted_trait', {ability: ability_index, trait: trait_index})
      })
    },
    delete_damage_type(state, form_data) {
      console.log("deleting " + form_data.name)
      var ability_index = _.findIndex(store.state.abilities, function(item) {
        return item.form.id === form_data.ability_id
      })

      var damage_type_index = _.findIndex(state.abilities[ability_index].data.damage_types, function(item) {
        return item.form.id === form_data.id
      })

      state.channel.push("delete_damage_type", form_data.id)
      .receive("ok", function() {
        console.log(form_data.name + " deleted")
        store.commit('remove_deleted_damage_type', {ability: ability_index, damage_type: damage_type_index})
      })
    },
    remove_deleted_trait(state, payload) {
      state.abilities[payload.ability].data.traits.splice(payload.trait, 1)
      state.abilities[payload.ability].form.traits.splice(payload.trait, 1)
    },
    remove_deleted_damage_type(state, payload) {
      state.abilities[payload.ability].data.damage_types.splice(payload.damage_type, 1)
      state.abilities[payload.ability].form.damage_types.splice(payload.damage_type, 1)
    },
    show_new_trait(state, ability_id) {
      var index = _.findIndex(store.state.abilities, function(ability) {
        return ability.data.id === ability_id
      })
      state.abilities[index].new_trait.show = true
    },
    show_new_damage_type(state, ability_id) {
      var index = _.findIndex(store.state.abilities, function(ability) {
        return ability.data.id === ability_id
      })
      state.abilities[index].new_damage_type.show = true
    },
    setAbilities(state, abilities) {
      state.abilities = abilities;
    },
    updatePagination(state, payload) {
      state.sortBy = payload.sortBy
      state.descending = payload.descending
      state.page = payload.page
    }
  }
})

window.abilities = new Vue({
  el: "#app",
  store,
  template: '#abilities',
  computed: {
    abilities: function() {
      var abilities = Object.values(this.$store.state.abilities)
      return abilities;
    }
  },
  methods: {
    createTrait: function(ability_id) {
      this.$store.commit('create_trait', ability_id)
    },
    createDamageType: function(ability_id) {
      this.$store.commit('create_damage_type', ability_id)
    },
    updatePageNumber: function(page) {
      this.$store.commit('updatePagination', {page: page, sortBy: this.$store.state.sortBy, descending: this.$store.state.descending})
      this.fetchPage()
    },
    updatePagination: function(pagination) {
      this.$store.commit('updatePagination', {page: this.$store.state.page, sortBy: pagination.sortBy, descending: !!pagination.descending})
      this.fetchPage()
    },
    fetchPage: function() {
      if (this.$store.state.channel.state === "joined") {
        abilities.loading = true
        var args = {query: abilities.search, page_number: this.$store.state.page, order_by: this.$store.state.sortBy, descending: this.$store.state.descending}
        this.$store.state.channel.push("fetch_page", args)
      }
    },
    showNewTrait: function(ability_id) {
      this.$store.commit('show_new_trait', ability_id)
    },
    showNewDamageType: function(ability_id) {
      this.$store.commit('show_new_damage_type', ability_id)
    },
    submit(form_data) {
      if (this.$refs["form-" + form_data.id].validate()) {
        this.$store.commit('update_ability', form_data)
      }
    },
    submitTrait(form_data) {
      if (this.$refs["trait-form-" + form_data.id][0].validate()) {
        this.$store.commit('update_trait', form_data)
      }
    },
    submitDamageType(form_data) {
      if (this.$refs["damage-type-form-" + form_data.id][0].validate()) {
        this.$store.commit('update_damage_type', form_data)
      }
    },
    deleteTrait(form_data) {
      this.$store.commit('delete_trait', form_data)
    },
    deleteDamageType(form_data) {
      this.$store.commit('delete_damage_type', form_data)
    },
    valid_targets: function() {
      return this.$store.state.valid_targets
    },
    kinds: function() {
      return this.$store.state.kinds
    },
    traits: function() {
      return this.$store.state.traits
    },
    damage_types: function() {
      return this.$store.state.damage_types
    },
    damageTypeHasChanged: function(damage_type) {
      var parse = function(string) {
        var result = null;
        try {
          result = JSON.parse(string);
        }
        catch(e) {
          if (string === "") {
            result = null
          }
          else {
            result = string
          }
        }
        return result
      }

      var data = {
        name: parse(damage_type.data.name),
        kind: parse(damage_type.data.kind),
        potency: parse(damage_type.data.potency)
      }
      var form = {
        name: parse(damage_type.form.name),
        kind: parse(damage_type.form.kind),
        potency: parse(damage_type.form.potency)
      }
      return !_.isEqual(data, form)
    },
    hasChanged: function(trait) {
      var parse = function(string) {
        var result = null;
        try {
          result = JSON.parse(string);
        }
        catch(e) {
          if (string === "") {
            result = null
          }
          else {
            result = string
          }
        }
        return result
      }

      var data = {
       name: parse(trait.data.name),
       value: parse(trait.data.value)
      }
      var form = {
        name: parse(trait.form.name),
        value: parse(trait.form.value)
       }
      return !_.isEqual(data, form)
    }
  },
  data() {
    return {
      pagination: {
        sortBy: 'id'
      },
      nameRules: [
        (v) => !!v || 'Name is required',
        (v) => !!v && v.length <= 30 || 'Name must be less than 30 characters'
      ],
      manaRules: [
        (v) => !!v || 'Mana is required',
        (v) => !!v && v >= 0 || 'Mana must greater than or equal to 0'
      ],
      durationRules: [
        (v) => !!("" + v) || 'Duration is required',
        (v) => !!v && v >= 0 || 'Duration must greater than or equal to 0'
      ],
      search: '',
      loading: true,
      headers: [
        {
          text: 'ID',
          align: 'left',
          value: 'id'
        },
        { text: 'Name', align: 'left', value: 'name' }
      ]
    }
  }
});

store.state.channel.join().receive("error", ({reason}) => window.location = "" + window.location.origin )

store.state.channel.on("abilities", function(data){
  abilities.loading = false
  var items = _.map(data.page.entries, function(ability) {
    var item = {
      form: JSON.parse(JSON.stringify(ability)),
      dialog: false,
      valid: true,
      data: JSON.parse(JSON.stringify(ability)),
      new_trait: {
        name: "",
        value: "",
        valid: false,
        show: false
      },
      new_damage_type: {
        name: "",
        potency: "",
        valid: false,
        show: false
      }
    }
    return item;
  })
  store.commit('setAbilities', items)
  store.state.valid_targets = data.valid_targets;
  store.state.kinds = data.kinds;
  store.state.traits = data.traits;
  store.state.damage_types = data.damage_types;
  abilities.pagination = {
    page: data.page.page_number,
    rowsPerPage: data.page.page_size,
    totalItems: data.page.total_entries,
    totalPages: data.page.total_pages
  }
});
